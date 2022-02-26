(defpackage org.nixos.lisp/main
  (:use :common-lisp
        :org.nixos.lisp/database/sqlite
        :org.nixos.lisp/repository/quicklisp
        :org.nixos.lisp/api))

(in-package org.nixos.lisp/main)

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
