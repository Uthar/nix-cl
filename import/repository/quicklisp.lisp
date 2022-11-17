(defpackage org.lispbuilds.nix/repository/quicklisp
  (:use :cl)
  (:import-from :dex)
  (:import-from :alexandria :read-file-into-string :ensure-list)
  (:import-from :alexandria-2 :line-up-first :line-up-last)
  (:import-from :arrow-macros :->>)
  (:import-from :str)
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
        (let ((path (fetchzip url hash)))
          (sqlite:execute-non-query db
            "insert or ignore into sha256(url,hash,path) values (?,?,?)"
            url hash path)))
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

      (let ((systems
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
                 )))

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
                  (nix-prefetch-tarball url db)
                (sql-query
                 "insert or ignore into system(name,version,asd) values (?,?,?)"
                 name version asd)
                (sql-query
                 "insert or ignore into sha256(url,hash,path) values (?,?,?)"
                 url hash path)
                (sql-query
                 "insert or ignore into src values
                  ((select id from sha256 where url=?),
                   (select id from system where name=? and version=?))"
                 url name version)
                (let ((meta (system-metadata name asd path)))
                  (apply #'sql-query "insert into meta values (?,?,?,?,?)" name meta)))))

          ;; Second pass: connect the in-database systems with
          ;; dependency information
          (dolist (system systems)
            (destructuring-bind (name version asd url deps) system
              (declare (ignore asd url))
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

(defun system-metadata (system asd src)
  (handler-case
      (progn
        ;; TODO(kasper): find asds in deeper directories
        (asdf:load-asd (make-pathname :directory src :name asd :type "asd"))
        (let* ((system (asdf:find-system system))
               (description (asdf:system-description system))
               (long-description (asdf:system-long-description system))
               (homepage (asdf:system-homepage system))
               (license (or (asdf:system-licence system)
                            (asdf:system-license system))))
          (map 'list #'write-to-string
               (list description long-description homepage license))))
    (error (e)
      (declare (ignore e))
      (list nil nil nil nil))))

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

(defvar *db*
  (sqlite:connect
   (make-pathname :name "tarballs"
                  :type "sqlite"
                  :defaults (cache-directory))))

(defvar *db2*
  (sqlite:connect "/home/kpg/scratch/nix-cl/packages.sqlite"))

(defparameter all-tarballs
(sqlite:execute-to-list
 *db2*
 "select distinct url from sha256"))


(sqlite:execute-non-query
 *db*
 "create table if not exists tarballs (url unique not null, path unique not null, hash not null, createtime default (julianday('now')))")

(sqlite:execute-non-query
 *db*
 "drop table tarballs")


(defun fetch-tarball (url)
  (let* ((tarball (dex:get url
                          :want-stream t
                          :force-binary t
                          :keep-alive t
                          :use-connection-pool t))
         (uri (quri:make-uri :defaults url))
         (path (alexandria-2:line-up-last
                (quri:uri-path uri)
                (split-sequence:split-sequence #\/)
                (alexandria:lastcar)))
         (name (alexandria-2:line-up-last
                (split-sequence:split-sequence #\. path)
                butlast
                (apply #'concatenate 'string)))
         (type (alexandria:lastcar
                (split-sequence:split-sequence #\. path)))
         (pathname (make-pathname
                    :name name
                    :type type
                    :defaults (tarball-cache-directory)))
         (file (open pathname
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8)))
         (digest (ironclad:make-digest :sha384))
         (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop for read = (read-sequence buf tarball)
               while (plusp read)
               do (progn
                    ;; (format t "read ~A bytes~%" read)
                    (write-sequence buf file :end read)
                    (ironclad:update-digest digest buf :end read))
               finally (sqlite:execute-non-query
                        *db*
                        "insert into tarballs (url,path,hash) values (?,?,?)"                           url
                        path
                        (concatenate
                              'string
                              "sha384-"
                              (cl-base64:usb8-array-to-base64-string
                               (ironclad:produce-digest digest)))))
      (close file)
      ;; (format t "Closed file stream~%")
      )))

    

                                    

                         

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
