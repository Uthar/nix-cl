(defsystem org.nixos.lisp
  :class :package-inferred-system
  :description "Utilities for importing ASDF systems into Nix"
  :depends-on (:alexandria
               :str
               :cl-ppcre
               :sqlite
               :dexador
               :arrow-macros
               :org.nixos.lisp/api
               :org.nixos.lisp/repository/quicklisp
               :org.nixos.lisp/database/sqlite
               ))


(register-system-packages
 "cl-ppcre"
 '(:ppcre))

(register-system-packages
 "dexador"
 '(:dex))
