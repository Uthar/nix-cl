(defpackage org.nixos.lisp/api
  (:documentation "Public interface of org.nixos.lisp")
  (:use :cl)
  (:export
   :import-lisp-packages
   :database->nix-expression))

(in-package org.nixos.lisp/api)

(defgeneric import-lisp-packages (repository database)
  (:documentation
   "Import Lisp packages (ASDF systems) from repository (Quicklisp,
   Ultralisp etc.) into a package database."))

(defgeneric database->nix-expression (database outfile)
  (:documentation
   "Generate a nix expression from the package database and write it
   into outfile."))
