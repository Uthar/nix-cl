(defpackage org.lispbuilds.nix/main
  (:use :common-lisp
        :org.lispbuilds.nix/database/sqlite
        :org.lispbuilds.nix/repository/quicklisp
        :org.lispbuilds.nix/api))

(in-package org.lispbuilds.nix/main)

(defvar *sqlite*
  (make-instance
   'sqlite-database
   :init-file "init.sql"
   :url "packages.sqlite"))

(defvar *quicklisp*
  (make-instance
   'quicklisp-repository
   :dist-url
   "https://beta.quicklisp.org/dist/quicklisp/2021-12-30/"))

(defun run-importers ()
  (import-lisp-packages *quicklisp* *sqlite*))

(defun gen-nix-file ()
  (database->nix-expression *sqlite* "out.nix"))

(defun main ()
  (run-importers)
  (gen-nix-file))
