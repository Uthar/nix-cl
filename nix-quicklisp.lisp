(require :asdf)
(asdf:load-system :alexandria)
(use-package :alexandria)
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)
(asdf:load-system :dexador)
(asdf:load-system :sqlite)

(defun nix-string (object)
  (format nil "\"~a\"" object))

(defun replace-regexes (from to str)
  (assert (= (length from) (length to)))
  (if (null from)
      str
      (replace-regexes
       (rest from)
       (rest to)
       (ppcre:regex-replace-all (first from) str (first to)))))

(defun nixify-symbol (string)
  (flet ((fix-special-chars (str)
           (replace-regexes '("[+]$" "[+][/]" "[+]" "[.]" "[/]")
                            '("_plus" "_plus/" "_plus_" "_dot_" "_slash_")
                            str)))
    (if (ppcre:scan "^[0-9]" string)
        (str:concat "_" (fix-special-chars string))
        (fix-special-chars string))))


(defun nix-symbol (object)
  (nixify-symbol (format nil "~a" object)))

(defun nix-eval (exp)
  (assert (consp exp))
  (ecase (car exp)
    (:string (nix-string (cdr exp)))
    (:list (nix-list (cdr exp)))
    (:funcall (apply #'nix-funcall (cdr exp)))
    (:attrs (nix-attrs (cdr exp)))
    (:merge (apply #'nix-merge (cdr exp)))
    (:symbol (nix-symbol (cdr exp)))))

(defun nix-list (things)
  (format nil "[ ~{~A~^ ~} ]" (mapcar 'nix-eval things)))

;; Path names are alphanumeric and can include the symbols +-._?= and
;; must not begin with a period.
(defun make-pname (string)
  (replace-regexes '("^[.]" "[^a-zA-Z0-9+-._?=]")
                   '("_" "_")
                   string))

(defvar *nix-attrs-depth* 0)

(defun nix-attrs (keyvals)
  (let ((*nix-attrs-depth* (1+ *nix-attrs-depth*)))
    (format nil
            (str:replace-all
             "*depth-1*"
             (str:repeat (1- *nix-attrs-depth*) "  ")
             (str:replace-all
              "*depth*"
              (str:repeat *nix-attrs-depth* "  ")
              "{~%*depth*~{~{~A = ~A;~}~^~%*depth*~}~%*depth-1*}"))
            (mapcar (lambda (keyval)
                      (let ((key (car keyval))
                            (val (cdr keyval)))
                        (list (nix-symbol key)
                              (nix-eval val))))
                    keyvals))))

(defun nix-funcall (fun args)
  (format nil "(~a ~{~a~^ ~})"
          (nixify-symbol fun)
          (mapcar 'nix-eval args)))

(defun nix-merge (a b)
  (format nil "(~a // ~b)"
          (nix-eval a)
          (nix-eval b)))

;; FIXME: preload these text files into a hash table/sqlite for faster acccess

(let ((systems-url "https://beta.quicklisp.org/dist/quicklisp/2021-12-30/systems.txt")
      (releases-url "https://beta.quicklisp.org/dist/quicklisp/2021-12-30/releases.txt"))
  (defvar *systems.txt* (butlast (str:split #\Newline (dex:get systems-url))))
  (defvar *releases.txt* (butlast (str:split #\Newline (dex:get releases-url)))))

(defvar *projects*
  (loop for line in *systems.txt*
        for words = (str:words line)
        collect `(:asd ,(second words)
                  :project ,(first words)
                  :system ,(third words)
                  :deps ,(nthcdr 3 words))))

(defun find-project (system)
  "Find the project to which `system` belongs."
  (loop for project in *projects*
        if (string= (getf project :system) system)
          do (return project)))

(defun find-release (project)
  "Find the release to which `project` belongs."
  (loop for line in *releases.txt*
        for words = (str:words line)
        if (string= (first words) project)
          do (return `(:project ,(first words)
                       :url ,(second words)
                       :sha1 ,(fifth words)
                       :version ,(str:replace-first (str:concat project "-") "" (sixth words))))))

(defvar *asds* (remove-duplicates
                (loop for line in *systems.txt*
                      collect (second (str:words line)))
                :test #'string=))

(defvar *systems* (remove-duplicates
                   (loop for line in *systems.txt*
                         collect (third (str:words line)))
                   :test #'string=))

(defun system-master (system)
  (first (str:split "/" system)))

(defun slashy? (system)
  (str:contains? "/" system))

(defun find-asd (system)
  "Find the asd to which `system` belongs."
  (getf (find-project system) :asd))

(defun parse-systems ()
  (loop for system in *systems*
        for master = (system-master system)
        for asd = (find-asd system)
        for project = (find-project system)
        for release = (find-release (getf project :project))
        for deps = (getf project :deps)
        collect `(:asd ,asd
                  :system ,system
                  :deps ,deps
                  :url ,(getf release :url)
                  :sha1 ,(getf release :sha1)
                  :version ,(getf release :version))))

(defvar *systems-parsed* (rest (parse-systems)))

(defun shell-command-to-string (cmd)
  (uiop:run-program cmd :output '(:string :stripped t)))

(defvar *sqlite* (sqlite:connect "quicklisp.sqlite"))
(sqlite:execute-non-query *sqlite* "create table if not exists sha256s (url unique, sha256)")

(defun ensure-sha256 (url)
  (or (sqlite:execute-single *sqlite* "select sha256 from sha256s where url=?" url)
      (let ((sha256 (shell-command-to-string (str:concat "nix-prefetch-url --unpack " url))))
        (sqlite:execute-single *sqlite* "insert into sha256s values (?, ?)" url sha256)
        sha256)))

(defun nix-prefetch-tarball (url)
  (restart-case
      (ensure-sha256 url)
    (try-again ()
      :report "Try downloading again"
      (nix-prefetch-tarball url))))

(defun nix-packages ()
  (funcall
   'nix-attrs
   (loop for pkg in *systems-parsed*
         for asd = (getf pkg :asd)
         for system = (getf pkg :system)
         for deps = (remove-if (lambda (dep) (string= dep "asdf")) (getf pkg :deps))
         for url = (getf pkg :url)
         for sha256 = (nix-prefetch-tarball url)
         for version = (getf pkg :version)
         collect (make-nix-package asd system version url sha256 deps))))

(defun make-nix-package (asd system version url sha256 deps)
  (let ((master (system-master system)))
    (cond ((and (slashy? system)
                (find master deps :test #'string=))
           `(,system
             . (:merge
                . ((:symbol . ,master)
                   (:attrs
                    . (("pname" . (:string . ,(make-pname system)))
                       ("systems"
                        . (:list
                           . ((:string . ,master)
                              (:string . ,system))))
                       ("lispLibs"
                        . (:list
                           . ,(mapcar
                               (lambda (dep) `(:symbol . ,dep))
                               (remove-if
                                (lambda (x)
                                  (or (string= x master)
                                      (string= x "asdf")))
                                (union
                                 (getf (find-project master) :deps)
                                 deps
                                 :test #'string=)))))))))))
          (t
           `(,system
             . (:attrs
                . (("pname" . (:string . ,(make-pname system)))
                   ("version" . (:string . ,version))
                   ("src"
                    . (:funcall
                       . ("createAsd"
                          ((:attrs
                            . (("url" . (:string . ,url))
                               ("sha256" . (:string . ,sha256))
                               ("system" . (:string . ,master))
                               ("asd" . (:string . ,asd))))))))
                   ("systems" . (:list . ((:string . ,system))))
                   ("lispLibs" . (:list . ,(mapcar (lambda (dep) `(:symbol . ,dep)) deps))))))))))

;;
;; FIXME Remove other system definitions than `system` from `asd`
;;

(defun write-nix-packages (outfile)
  (with-open-file (stream outfile :direction :output :if-exists :supersede)
    (format stream "
# This file was auto-generated by nix-quicklisp.lisp

{ pkgs, ... }:

with builtins;

let createAsd = { url, sha256, asd, system }:
   let
     src = fetchTarball { inherit url sha256; };
   in pkgs.runCommand \"source\" {} ''
      mkdir -pv $out
      cp -r ${src}/* $out
      find $out -name \"${asd}.asd\" | while read f; do mv -fv $f $(dirname $f)/${system}.asd || true; done
  '';
in

rec ~a" (nix-packages))))

(write-nix-packages "from-quicklisp.nix")
