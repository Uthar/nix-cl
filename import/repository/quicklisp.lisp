(defpackage org.lispbuilds.nix/repository/quicklisp
  (:use :cl)
  (:import-from :dex)
  (:import-from :alexandria :read-file-into-string :ensure-list)
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
  (:export :quicklisp-repository))

(in-package org.lispbuilds.nix/repository/quicklisp)

(defclass quicklisp-repository ()
  ((dist-url :initarg :dist-url
             :reader dist-url
             :initform (error "dist url required"))))

(defmethod import-lisp-packages ((repository quicklisp-repository)
                                 (database sqlite-database))
  (let* ((db (sqlite:connect (database-url database)))
         (systems-url (str:concat (dist-url repository) "systems.txt"))
         (releases-url (str:concat (dist-url repository) "releases.txt"))
         (systems-lines (rest (butlast (str:split #\Newline (dex:get systems-url)))))
         (releases-lines (rest (butlast (str:split #\Newline (dex:get releases-url))))))
    (flet ((run-sql (sql &rest params)
             (apply #'sqlite:execute-non-query `(,db ,sql ,@params)))
           (query-sql (sql &rest params)
             (apply #'sqlite:execute-to-list `(,db ,sql ,@params))))

      ;; Ensure database schema
      (init-db db (init-file database))

      ;; Prepare temporary tables for efficient access
      (run-sql "create temp table if not exists quicklisp_system
                (project, asd, name unique, dependencies)")

      (run-sql "create temp table if not exists quicklisp_release
                (project unique, url, size, md5, sha1, prefix, asds)")

      (sqlite:with-transaction db
        (dolist (line systems-lines)
          (destructuring-bind (project asd name &rest deps)
              (str:words line)
            (run-sql
             "insert or ignore into quicklisp_system values(?,?,?,?)"
             project asd name (str:join #\, deps)))))

      (sqlite:with-transaction db
        (dolist (line releases-lines)
          (destructuring-bind (project url size md5 sha1 prefix &rest asds)
              (str:words line)
            (run-sql
             "insert or ignore into quicklisp_release values(?,?,?,?,?,?,?)"
             project url size md5 sha1 prefix (str:join #\, asds)))))

      (sqlite:with-transaction db
        (let ((systems
                (query-sql
                 "select s.project, s.name, r.prefix, s.asd, r.url
                  from quicklisp_system s, quicklisp_release r
                  where s.project=r.project"
                 ))
              (dependencies
                (query-sql
                 "select name, dependencies from quicklisp_system"
                 )))
          (dolist (system systems)
            (destructuring-bind (project name prefix asd url)
                system
              (let* ((name name)
                     (version (str:replace-first (str:concat project "-") "" prefix))
                     (asd asd)
                     (url url)
                     (hash (nix-prefetch-tarball url db)))
                (run-sql
                 "insert or ignore into system(name,version,asd) values (?,?,?)"
                 name version asd)
                (run-sql
                 "insert or ignore into sha256(url,hash) values (?,?)"
                 url hash)
                (run-sql
                 "insert or ignore into src values
                  ((select id from sha256 where url=?),
                   (select id from system where name=?))"
                 url name))))
          (dolist (dependency dependencies)
            (destructuring-bind (name dependencies)
                dependency
              (dolist (system (str:split #\, dependencies))
                (run-sql
                 "insert or ignore into dep values
                  ((select id from system where name=?),
                   (select id from system where name=?))"
                 name system)))))))))

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
  (or (sqlite:execute-single db "select hash from sha256 where url=?" url)
      (let ((sha256 (shell-command-to-string (str:concat "nix-prefetch-url --unpack " url))))
        sha256)))
