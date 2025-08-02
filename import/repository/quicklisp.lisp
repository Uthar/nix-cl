(defpackage org.lispbuilds.nix/repository/quicklisp
  (:use :cl)
  (:import-from :dex)
  (:import-from :alexandria :read-file-into-string :ensure-list :when-let :ensure-gethash)
  (:import-from :arrow-macros :->>)
  (:import-from :str)
  (:import-from :tar)
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
   :make-count-down-latch
   :count-down
   :await
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

(defvar *db* nil
  "Current SQLite connection")

;; TODO: This should not know about the imported.nix file.
(defun init-tarball-hashes (database)
  (status "no packages.sqlite - will pre-fill tarball hashes from ~A to save time~%"
          (truename "imported.nix"))
  (let* ((lines (uiop:read-file-lines "imported.nix"))
         (lines (remove-if-not
                  (lambda (line)
                    (let ((trimmed (str:trim-left line)))
                      (or (str:starts-with-p "url = " trimmed)
                          (str:starts-with-p "sha256 = " trimmed))))
                  lines))
         (lines (mapcar
                 (lambda (line)
                   (multiple-value-bind (whole groups)
                       (ppcre:scan-to-strings "\"\(.*\)\"" line)
                     (declare (ignore whole))
                     (svref groups 0)))
                 lines)))
    (sqlite:with-open-database (*db* (database-url database))
      (init-db *db* (init-file database))
      (sqlite:with-transaction *db*
        (loop while lines do
          (sqlite:execute-non-query *db*
            "insert or ignore into sha256(url,hash) values (?,?)"
            (prog1 (first lines) (setf lines (rest lines)))
            (prog1 (first lines) (setf lines (rest lines))))))
      (status "OK, imported ~A hashes into DB.~%"
              (sqlite:execute-single *db*
                 "select count(*) from sha256")))))

(defun sql-query (sql &rest params)
  (apply #'sqlite:execute-to-list (list* *db* sql params)))

(defmethod import-lisp-packages ((repository quicklisp-repository)
                                 (database sqlite-database))

  ;; If packages.sqlite is missing, we should populate the sha256
  ;; table to speed things up.
  (unless (probe-file (database-url database))
    (init-tarball-hashes database))

  (let* ((*db* (sqlite:connect (database-url database)))
         (systems-url (str:concat (dist-url repository) "systems.txt"))
         (releases-url (str:concat (dist-url repository) "releases.txt"))
         (systems-lines (rest (butlast (str:split #\Newline (dex:get systems-url)))))
         (releases-lines (rest (butlast (str:split #\Newline (dex:get releases-url))))))

    (progn

      ;; Ensure database schema
      (init-db *db* (init-file database))

      ;; Prepare temporary tables for efficient access
      (sql-query "create temp table if not exists quicklisp_system
                  (project, asd, name unique, deps)")

      (sql-query "create temp table if not exists quicklisp_release
                  (project unique, url, size, md5, sha1, prefix not null, asds)")

      (sqlite:with-transaction *db*
        (dolist (line systems-lines)
          (destructuring-bind (project asd name &rest deps)
              (str:words line)
            (sql-query
             "insert or ignore into quicklisp_system values(?,?,?,?)"
             project asd name (json:stringify (coerce deps 'vector))))))

      (sqlite:with-transaction *db*
        (dolist (line releases-lines)
          (destructuring-bind (project url size md5 sha1 prefix &rest asds)
              (str:words line)
            (sql-query
             "insert or ignore into quicklisp_release values(?,?,?,?,?,?,?)"
             project url size md5 sha1 prefix (json:stringify (coerce
                                                               asds
                                                               'vector))))))

      (progn
        ;; Should these be temp tables, that then get queried by
        ;; system name? This looks like it uses a lot of memory.
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
                  from pkg where name not in (select name from system)"
                 )))

          ;; First pass: insert system and source tarball informaton.
          ;; Can't insert dependency information, because this works
          ;; on system ids in the database and they don't exist
          ;; yet. Could it be better to just base dependencies on
          ;; names? But then ACID is lost.
          (let ((tgz-jobs (sb-concurrency:make-mailbox))
                (tgz-done (sb-concurrency:make-mailbox))
                (tgz-failed (sb-concurrency:make-mailbox))
                (asd-jobs (sb-concurrency:make-mailbox))
                (asd-done (sb-concurrency:make-mailbox))
                (asd-failed (sb-concurrency:make-mailbox))
                (*bus* (sb-concurrency:make-mailbox))
                (scrape-done (make-count-down-latch :count (length systems))))
            (tgz-scheduler)
            (asd-scheduler)
            (retry-sheduler)
            (tgz-scraper)
            (asd-scraper)
            (tgz-loader)
            (asd-loader)
            (dolist (system systems)
              (sb-concurrency:send-message tarball-jobs system))
            (sb-concurrency:send-message tarball-jobs nil)
            (await scrape-done))

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
                    dep-name dep-version))))))))))
  (write-char #\Newline *error-output*))

(defun download-tarballs (jobs scrape-jobs)
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (let ((limit (bt:make-semaphore :count 2)))
            (loop for job = (sb-concurrency:receive-message jobs) while job do
              (bt:wait-on-semaphore limit)
              (bt:make-thread
               (destructuring-bind (name version asd url deps) job
                 (declare (ignore deps))
                 (lambda ()
                   (status "importing system BLA '~a-~a'" name version)
                   (let (tgz-path)
                     (unwind-protect
                          (let ((res (fetch-tarball url)))
                            (destructuring-bind (hash path) res
                              (setf tgz-path (truename path))
                              (sql-query
                               "insert or ignore into sha256(url,hash,path) values (?,?,?)"
                               url hash path)))
                       (bt:signal-semaphore limit))
                     (unwind-protect
                          (sql-query
                           "insert or ignore into system(name,version,asd) values (?,?,?)"
                           name version asd)
                       (sb-concurrency:send-message scrape-jobs (cons tgz-path job))))
                   (sql-query
                    "insert or ignore into src values
                 ((select id from sha256 where url=?),
                  (select id from system where name=? and version=?))"
                    url name version))))))
       (format t "tgz downloading done.~%")
       (sb-concurrency:send-message scrape-jobs nil)))))

(defun scrape-asds (jobs done)
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (let ((retries (make-hash-table :test 'equal :synchronized t)))
            (loop for job = (sb-concurrency:receive-message jobs) while job do
              (destructuring-bind (path name version asd url deps) job
                (declare (ignore url deps))
                (let (mark-done)
                  (unwind-protect
                       (handler-case
                           (let (sys found-asd short long license homepage)
                             (setf found-asd (find-file asd path))
                             (asdf:load-asd found-asd)
                             (setf sys (asdf:find-system name t))
                             (setf short (asdf:system-description sys))
                             ;; It's too long - whole README sometimes
                             ;; (setf long (asdf:system-long-description sys))
                             (when-let ((lic (or (asdf:system-licence sys)
                                                 (asdf:system-license sys))))
                               (setf license (string lic))) ;sometimes a keyword
                             (when-let ((home (asdf:system-homepage sys)))
                               (when (stringp home) ;sometimes a symbol
                                 (setf homepage home)))
                             (sql-query
                              "insert or replace into meta values
                          ((select id from system where name=? and version=?)
                           ,?,?,?,?)"
                              name version homepage license short long)
                             (setf mark-done t))
                         (file-not-found (e)
                           (format t "E: ~A~%" e)
                           (setf mark-done t))
                         (asdf:missing-dependency (e)
                           (format t "E: ~A~%" e)
                           (ensure-gethash name retries 0)
                           (if (<= (incf (gethash name retries)) 10)
                               (sb-concurrency:send-message jobs job)
                               (setf mark-done t)))
                         (asdf:missing-component (e)
                           (format t "E: ~A~%" e)
                           (setf mark-done t))
                         (error (e)
                           (format t "E: ~A~%" e)
                           (ensure-gethash name retries 0)
                           (if (<= (incf (gethash name retries)) 10)
                               (sb-concurrency:send-message jobs job)
                               (setf mark-done t))))
                    (when mark-done
                      (count-down done)))))))
       (format t "asd scraping done.~%")))))

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

(defun fetch-tarball (url)
  (restart-case
      (prefetch-tarball-cached url)
    (try-again ()
      :report "Try downloading again"
      (fetch-tarball url))))

(defvar *up-for-retry*
  (make-hash-table :test 'equal :synchronized t))

(defun find-file (what where)
  (or
   (dolist (file (uiop:directory-files where))
     (when (and (string= (pathname-name file) (pathname-name what))
                (string= (pathname-type file) (pathname-type what)))
       (return file)))
   (dolist (dir (uiop:subdirectories where))
     (when-let ((found (find-file what dir)))
       (return found)))))

(define-condition file-not-found (error)
  ((file :initarg :file :reader .file)))

(defun analyze-tarball (url system asd)
  (multiple-value-bind (hash path)
      (nix-prefetch-tarball url)
    (restart-case
        (let (sys res found-asd)
          (setf found-asd (find-file asd path))
          (when (null found-asd)
            (error 'file-not-found :file asd))
          (asdf:load-asd found-asd)
          (setf sys (asdf:find-system system t))
          (setf (getf res :hash) hash)
          (when-let ((short (asdf:system-description sys)))
            (setf (getf res :short) short))
          (when-let ((long (asdf:system-long-description sys)))
            (setf (getf res :long) long))
          (when-let ((lic (or (asdf:system-licence sys)
                              (asdf:system-license sys))))
            ;; sometimes a keyword
            (setf (getf res :license) (string lic)))
          (when-let ((home (asdf:system-homepage sys)))
            (setf (getf res :home) home))
          res)
      (retry ()
        (format t "Retrying to get metadata about: ~A~%" system)
        (analyze-tarball url system asd))
      (retry-later (c)
        (format t "Will try to scrape ~A after its defsystem dependencies are loaded~%" asd)
        (vector-push-extend
         (list :url url :system system :asd asd)
         (ensure-gethash (asdf/find-component:missing-requires c)
                         *up-for-retry*
                         (make-array 4 :adjustable t :fill-pointer 0)))
        (list :hash hash))
      (skip ()
        (format t "Will not scrape metadata about: ~A~%" system)
        (list :hash hash)))))

(declaim (optimize (debug 3)))

(defun prefetch-tarball-cached (url)
  (or (first (sqlite:execute-to-list *db* "select hash, path from sha256 where url = ?;" url))
      (let* ((cmd (str:concat "nix-prefetch-url --print-path --unpack " url))
             (res (shell-command-to-string cmd)))
        (destructuring-bind (hash path)
            (uiop:split-string res :separator '(#\Newline))
          (list hash path)))))
