(defpackage org.nixos.lisp/repository/quicklisp
  (:use :cl)
  (:import-from :dex)
  (:import-from :alexandria :read-file-into-string :ensure-list)
  (:import-from :arrow-macros :->>)
  (:import-from :str)
  (:import-from
   :org.nixos.lisp/database/sqlite
   :sqlite-database
   :init-db
   :database-url
   :init-file)
  (:import-from
   :org.nixos.lisp/api
   :import-lisp-packages)
  (:export :quicklisp-repository))

(in-package org.nixos.lisp/repository/quicklisp)

(defclass quicklisp-repository ()
  ((dist-url :initarg :dist-url
             :reader dist-url
             :initform (error "dist url required"))))

(defun replace-regexes (from to str)
  (assert (= (length from) (length to)))
  (if (null from)
      str
      (replace-regexes
       (rest from)
       (rest to)
       (ppcre:regex-replace-all (first from) str (first to)))))

(defmethod import-lisp-packages ((repository quicklisp-repository)
                                 (database sqlite-database))
  (let* ((db (sqlite:connect (database-url database)))
         (systems-url (str:concat (dist-url repository) "systems.txt"))
         (releases-url (str:concat (dist-url repository) "releases.txt"))
         (systems-lines (rest (butlast (str:split #\Newline (dex:get systems-url)))))
         (releases-lines (rest (butlast (str:split #\Newline (dex:get releases-url))))))
    (flet ((run-sql (sql &rest params)
             (apply #'sqlite:execute-non-query `(,db ,sql ,@params))))

      ;; Ensure database schema
      (init-db db (init-file database))

      ;; Create quicklisp-specific metadata table
      (run-sql "create table if not exists quicklisp_metadata (system_id, project)")

      ;; Insert systems first so that we can insert their dependencies
      (sqlite:with-transaction db
        (dolist (line systems-lines)
          (destructuring-bind (project asd name &rest deps) (str:words line)
            (declare (ignorable deps))
            (run-sql "insert or ignore into system (name, asd) values (?, ?)"
                     name asd)
            (run-sql "insert or ignore into quicklisp_metadata (system_id, project)
                    values ((select id from system where name = ?), ?)"
                     name project))))

      ;; Insert the dependencies, now that the foreign keys will work
      (sqlite:with-transaction db
        (dolist (line systems-lines)
          (destructuring-bind (project asd name &rest deps) (str:words line)
            (declare (ignorable project asd))
            (dolist (dep deps)
              (run-sql "insert or ignore into dep (system_id, dep_id)
                      values ((select id from system where name = ?),
                              (select id from system where name = ?))"
                       name dep)))))

      ;; Download sources, compute their sha256 hashes and save them
      (sqlite:with-transaction db
        (dolist (line releases-lines)
          (destructuring-bind (project url &rest _)
              (str:words line)
            (declare (ignorable project _))
            (run-sql "insert or ignore into sha256 (url, hash) values (?, ?)"
                     url (nix-prefetch-tarball url db)))))

      ;; Insert source information
      (sqlite:with-transaction db
        (dolist (line releases-lines)
          (destructuring-bind (project url &rest _)
              (str:words line)
            (declare (ignorable _))
            (run-sql "insert or ignore into src (sha256_id, system_id)
                    values ((select id from sha256 where url = ?),
                            (select system_id
                             from quicklisp_metadata
                             where project = ?))"
                     url project)))))))


(defun shell-command-to-string (cmd)
  (uiop:run-program cmd :output '(:string :stripped t)))

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
