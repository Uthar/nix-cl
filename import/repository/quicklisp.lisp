(defpackage org.lispbuilds.nix/repository/quicklisp
  (:use :cl)
  (:import-from :dex)
  (:import-from :alexandria :read-file-into-string :ensure-list)
  (:import-from :alexandria-2 :line-up-first :line-up-last)
  (:import-from :arrow-macros :->>)
  (:import-from :str)
  (:import-from :tar)
  (:import-from :tar-simple-extract)
  (:import-from :ironclad)
  (:import-from
   :org.lispbuilds.nix/pool
   :thread-pool
   :submit
   :shutdown
   :job)
  (:import-from
   :org.lispbuilds.nix/database/sqlite
   :sqlite-database
   :init-db
   :database-url
   :init-file)
  (:import-from
   :org.lispbuilds.nix/api
   :import-lisp-packages)
  (:import-from
   :org.lispbuilds.nix/util
   :replace-regexes)
  (:export :quicklisp-repository)
  (:local-nicknames
   (:json :com.inuoe.jzon)))

(in-package org.lispbuilds.nix/repository/quicklisp)

(declaim (optimize debug))

(defclass quicklisp-repository ()
  ((dist-url :initarg :dist-url
             :reader dist-url
             :initform (error "dist url required"))))

(defun clear-line ()
  (write-char #\Return *error-output*)
  (write-char #\Escape *error-output*)
  (write-char #\[ *error-output*)
  (write-char #\K *error-output*))

(defun status (&rest format-args)
  (clear-line)
  (apply #'format (list* *error-output* format-args))
  (force-output *error-output*))

;; TODO: This should not know about the imported.nix file.
(defun init-tarball-hashes (database)
  (status "no packages.sqlite - will pre-fill tarball hashes from ~A to save time~%"
          (truename "imported.nix"))
  (let ((lines (line-up-last
                (uiop:read-file-lines "imported.nix")
                (remove-if-not
                 (lambda (line)
                   (let ((trimmed (str:trim-left line)))
                     (or (str:starts-with-p "url = " trimmed)
                         (str:starts-with-p "sha256 = " trimmed)))))
                (mapcar
                 (lambda (line)
                   (multiple-value-bind (whole groups)
                       (ppcre:scan-to-strings "\"\(.*\)\"" line)
                     (declare (ignore whole))
                     (svref groups 0))))))
        )
    (assert (evenp (length lines)))
    (sqlite:with-open-database (db (database-url database))
      (init-db db (init-file database))
      (do ((lines lines (rest (rest lines)))
           (url (first lines) (first lines))
           (hash (second lines) (second lines)))
          ((null lines))
        (sqlite:execute-non-query db
                                  "insert or ignore into sha256(url,hash) values (?,?)"
                                  url hash))
      (status "OK, imported ~A hashes into DB.~%"
              (sqlite:execute-single db
                                     "select count(*) from sha256")))))

(defmethod import-lisp-packages ((repository quicklisp-repository)
                                 (database sqlite-database))

  ;; If packages.sqlite is missing, we should populate the sha256
  ;; table to speed things up.
  (unless (probe-file (database-url database))
    (init-tarball-hashes database))

  (let* ((db (sqlite:connect (database-url database)))
         (systems-url (str:concat (dist-url repository) "systems.txt"))
         (releases-url (str:concat (dist-url repository) "releases.txt"))
         (systems-lines (rest (butlast (str:split #\Newline (dex:get systems-url)))))
         (releases-lines (rest (butlast (str:split #\Newline (dex:get releases-url))))))

    (flet ((sql-query (sql &rest params)
             (apply #'sqlite:execute-to-list (list* db sql params))))

      ;; Ensure database schema
      (init-db db (init-file database))

      ;; Prepare temporary tables for efficient access
      (sql-query "create temp table if not exists quicklisp_system
                  (project, asd, name unique, deps)")

      (sql-query "create temp table if not exists quicklisp_release
                  (project unique, url, size, md5, sha1, prefix not null, asds)")

      (sqlite:with-transaction db
        (dolist (line systems-lines)
          (destructuring-bind (project asd name &rest deps)
              (str:words line)
            (sql-query
             "insert or ignore into quicklisp_system values(?,?,?,?)"
             project asd name (json:stringify (coerce deps 'vector))))))

      (sqlite:with-transaction db
        (dolist (line releases-lines)
          (destructuring-bind (project url size md5 sha1 prefix &rest asds)
              (str:words line)
            (sql-query
             "insert or ignore into quicklisp_release values(?,?,?,?,?,?,?)"
             project url size md5 sha1 prefix (json:stringify (coerce
                                                               asds
                                                               'vector))))))

      (let* ((systems
               (sql-query
                "with pkg as (
                    select
                      name, asd, url, deps,
                      ltrim(replace(prefix, r.project, ''), '-_') as version
                    from quicklisp_system s, quicklisp_release r
                    where s.project = r.project
                  )
                  select
                    name, version, asd, url,
                    (select json_group_array(
                       json_array(value, (select version from pkg where name=value))
                     )
                     from json_each(deps)) as deps
                  from pkg"
                ))
             (tarball-urls (map 'list #'fourth systems)))

        (fetch-tarballs tarball-urls)

        ;; First pass: insert system and source tarball informaton.
        ;; Can't insert dependency information, because this works
        ;; on system ids in the database and they don't exist
        ;; yet. Could it be better to just base dependencies on
        ;; names? But then ACID is lost.
        (dolist (system systems)
          (destructuring-bind (name version asd url deps) system
            (declare (ignore deps))
            (status "importing system '~a-~a'" name version)
            (multiple-value-bind (hash path)
                (fetch-tarball url)
              (sql-query
               "insert or ignore into system(name,version,asd) values (?,?,?)"
               name version asd)
              (sql-query
               "insert or ignore into sha256(url,hash) values (?,?)"
               url hash)
              (sql-query
               "insert or ignore into src values
                ((select id from sha256 where url=?),
                 (select id from system where name=? and version=?))"
               url name version)
              (let ((meta (system-metadata url asd name)))
                (apply #'sql-query "insert into meta values (?,?,?,?,?)" name meta)))))

        ;; Second pass: connect the in-database systems with
        ;; dependency information
        (dolist (system systems)
          (destructuring-bind (name version asd url deps) system
            (declare (ignore asd url))
            (status "setting dependencies of '~a-~a'" name version)
            (dolist (dep (coerce (json:parse deps) 'list))
              (destructuring-bind (dep-name dep-version) (coerce dep 'list)
                (if (eql dep-version 'NULL)
                    (warn "Bad data in Quicklisp: ~a has no version" dep-name)
                    (sql-query
                     "insert or ignore into dep values
                     ((select id from system where name=? and version=?),
                      (select id from system where name=? and version=?))"
                     name version
                     dep-name dep-version)))))))))

  (write-char #\Newline *error-output*))

(defun shell-command-to-string (cmd)
  ;; Clearing the library path is needed to prevent a bug, where the
  ;; called subprocess uses a different glibc than the SBCL process
  ;; is. In that case, the call to execve attempts to load the
  ;; libraries used by SBCL from LD_LIBRARY_PATH using a different
  ;; glibc than they expect, which errors out.
  (let ((ld-library-path  (uiop:getenv "LD_LIBRARY_PATH")))
    (setf (uiop:getenv "LD_LIBRARY_PATH") "")
    (unwind-protect
         (uiop:run-program cmd :output '(:string :stripped t))
      (setf (uiop:getenv "LD_LIBRARY_PATH") ld-library-path))))

(defun nix-prefetch-tarball (url db)
  (restart-case
      (compute-sha256 url db)
    (try-again ()
      :report "Try downloading again"
      (nix-prefetch-tarball url db))))

(defun compute-sha256 (url db)
  (destructuring-bind (sha256 path)
      (or (first (sqlite:execute-to-list db "select hash, path from sha256 where url=?" url))
          (str:split #\Newline
                     (shell-command-to-string (str:concat "nix-prefetch-url --unpack --print-path " url))))
    (values sha256 path)))

(defun find-file (dir name type)
  (let ((file nil))
    (uiop:collect-sub*directories
     dir 
     (lambda (_) (null file))
     (lambda (_) (null file))
     (lambda (path)
       (let ((files (directory (make-pathname
                                :defaults path
                                :name :wild
                                :type :wild))))
         (setf file
               (find-if
                (lambda (path)
                  (and (string= (pathname-name path) name)
                       (string= (pathname-type path) type)))
                files)))))
    file))

(defun find-files (dir name type)
  (let ((all-files (list nil)))
    (uiop:collect-sub*directories
     dir 
     (constantly t)
     (constantly t)
     (lambda (path)
       (let ((files (directory (make-pathname
                                :defaults path
                                :name :wild
                                :type :wild))))
         (nconc all-files
                (remove-if-not
                 (lambda (path)
                   (and (string= (pathname-name path) name)
                        (string= (pathname-type path) type)))
                 files)))))
    (rest all-files)))

(defun system-metadata (url asd system)
  (handler-case
      (let ((tmpdir (uiop:temporary-directory)))
        (multiple-value-bind (hash path)
            (fetch-tarball url)
          (declare (ignore hash))
          (ignore-errors (sb-ext:delete-directory tmpdir :recursive t))
          (ensure-directories-exist tmpdir)
          (tar:with-open-archive (a path)
            (tar-simple-extract:simple-extract-archive
             a
             :directory tmpdir
             :strip-components 1
             :if-exists :supersede))
        (asdf:load-asd (find-file tmpdir asd "asd"))
        (let* ((system (asdf:find-system system))
               (description (asdf:system-description system))
               (long-description (asdf:system-long-description system))
               (homepage (asdf:system-homepage system))
               (license (or (asdf:system-licence system)
                            (asdf:system-license system))))
          (sb-ext:delete-directory tmpdir :recursive t)
          (map 'list #'write-to-string
               (list description long-description homepage license)))))
    (error (e)
      (format *error-output* "~%Error while getting metadata: ~A~%" (substitute #\Space #\Newline (format nil "~A" e)))
      (list nil nil nil nil))
    (warning (w)
      (declare (ignore w))
      (values))))

(defparameter +quotes+ (make-string 1 :initial-element #\"))


(defun cache-directory ()
  (let ((pathname (merge-pathnames
                   (make-pathname :directory
                                  '(:relative "nix-cl"))
                   (uiop:xdg-cache-home))))
    (ensure-directories-exist pathname)
    pathname))

(defun tarball-cache-directory ()
  (let ((pathname (merge-pathnames
                   (make-pathname :directory
                                  '(:relative "tarballs"))
                   (cache-directory))))
    (ensure-directories-exist pathname)
    pathname))

(defvar *db* nil)

(defvar *log-lock* (bt:make-lock))

(defvar *db-lock* (bt:make-lock))

(defvar *retries* 0)
(defun http-get (url)
  (handler-case
      (dex:get url 
               :want-stream t
               :force-binary t
               :keep-alive t
               :use-connection-pool t)
    (dexador:http-request-failed (e)
      (when (< *retries* 5)
        (let ((*retries* (1+ *retries*)))
          (bt:with-lock-held (*log-lock*)
            (format t "Retry x~A after ~A~%" *retries* e))
          (sleep (* 0.2 *retries*))
          (http-get url))))))

(defun fetch-tarball (url)
  (let ((exists
          (bt:with-lock-held (*db-lock*)
            ;; TODO use journal_mode=wal
            (sqlite:execute-single *db* "select * from tarballs where url=?" url))))
    (when exists
      (bt:with-lock-held (*log-lock*)
        (status "<~A>: CACHE ~A" (bt:thread-name (bt:current-thread)) url)))
    (if exists
        (destructuring-bind (hash name)
            (first (sqlite:execute-to-list *db* "select hash, path from tarballs where url=?" url))
          (values hash (make-pathname
                        :name (pathname-name name)
                        :type (pathname-type name)
                        :defaults (tarball-cache-directory))))
        (progn
      (bt:with-lock-held (*log-lock*)
        (status "<~A>: FETCH ~A" (bt:thread-name (bt:current-thread)) url))
      (let* ((tarball (http-get url))
             (uri (quri:make-uri :defaults url))
             (path (let ((path (quri:uri-path uri)))
                     (subseq path (1+ (position #\/ path :from-end t)))))
             (name (subseq path 0 (position #\. path :from-end t)))
             (type (subseq path (1+ (position #\. path :from-end t))))
             (pathname (make-pathname
                        :name name
                        :type type
                        :defaults (tarball-cache-directory)))
             (file (open pathname
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8)))
             (digest (ironclad:make-digest :sha256))
             (buf (make-array 4096 :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent buf))
        ;; (format t "~A ~A ~A ~A~%" name type pathname path)
        (unwind-protect
             (loop for read = (read-sequence buf tarball)
                   while (plusp read)
                   do (progn
                        (write-sequence buf file :end read)
                        (ironclad:update-digest digest buf :end read))
                   finally (return (let ((hash (concatenate
                                        'string
                                        (cl-base64:usb8-array-to-base64-string
                                         (ironclad:produce-digest digest)))))
                             (prog1
                                 (values hash pathname)
                               (bt:with-lock-held (*db-lock*)
                                 (sqlite:execute-non-query
                                  *db*
                                  "insert into tarballs (url,path,hash) values (?,?,?)"
                                  url path hash))))))
          (close file)
          ;; (format t "Closed file stream~%")
          ))))))

(defvar *pool* nil)

(defun fetch-tarballs (urls)
  (setf dexador.connection-cache:*max-active-connections* 25)
  (dexador.connection-cache::make-new-connection-pool)
  (setf *pool* (make-instance 'thread-pool :size 25))

  (setf *db*
        (sqlite:connect
         (make-pathname :name "tarballs"
                        :type "sqlite"
                        :defaults (cache-directory))
         :busy-timeout 5))

  (sqlite:execute-non-query *db* "pragma journal_mode=wal")

  (sqlite:execute-non-query
   *db*
   "create table if not exists tarballs
    (url unique not null,
    path unique not null,
    hash not null,
    createtime default (julianday('now')))")

  (dolist (url (remove-duplicates urls :test #'string=))
    (let ((job (make-instance 'job
                              :fn (lambda (x)
                                    (fetch-tarball x))
                              :args (list url))))
      (submit *pool* job)))
    
  (dexador:clear-connection-pool)
  (shutdown *pool*)
  ;; (sqlite:disconnect *db*)
  )

                         

(defun fetchzip (url hash)
  (status "fetching ~A" url)
  (shell-command-to-string
   (format nil
     "
     nix build --print-out-paths --impure --expr '  \
        let                                         \
          nixpkgs = builtins.getFlake ~a;           \
          pkgs = builtins.getAttr                   \
            builtins.currentSystem                  \
            nixpkgs.legacyPackages;                 \
        in pkgs.fetchzip {                          \
          url = ~a;                                 \
          sha256 = ~a;                              \
        }                                           \
     '
     "
     (concatenate 'string +quotes+ "nixpkgs" +quotes+)
     (concatenate 'string +quotes+ url +quotes+)
     (concatenate 'string +quotes+ hash +quotes+))))

;; (fetchzip "http://beta.quicklisp.org/archive/zpng/2015-04-07/zpng-1.2.2.tgz" "0b3ag3jhl3z7kdls3ahdsdxsfhhw5qrizk769984f4wkxhb69rcm")
