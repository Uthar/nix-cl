(defsystem org.lispbuilds.nix
  :class :package-inferred-system
  :description "Utilities for importing ASDF systems into Nix"
  :depends-on (
               :alexandria
               :str
               :cl-ppcre
               :sqlite
               :tar
               :dexador
               :arrow-macros
               :com.inuoe.jzon
               :sb-concurrency
               :org.lispbuilds.nix/api
               :org.lispbuilds.nix/repository/quicklisp
               :org.lispbuilds.nix/database/sqlite
               ))


(register-system-packages
 "cl-ppcre"
 '(:ppcre))

(register-system-packages
 "dexador"
 '(:dex))

(register-system-packages
 "alexandria"
 '(:alexandria :alexandria-2))
