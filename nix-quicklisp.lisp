(require :asdf)
(asdf:load-system :str)
(asdf:load-system :cl-ppcre)
(asdf:load-system :dexador)
(asdf:load-system :sqlite)

(defun nix-string (object)
  (format nil "\"~a\"" object))

(defun nixify-symbol (string)
  (if (ppcre:scan "^[0-9]" string)
      (str:concat "_" (ppcre:regex-replace-all "[.+]" string "_"))
      (ppcre:regex-replace-all "[.+]" string "_")))

(defun nix-symbol (object)
  (nixify-symbol (format nil "~a" object)))

(defun nix-eval (exp)
  (ecase (car exp)
    (:string (nix-string (cdr exp)))
    (:list (nix-list (cdr exp)))
    (:funcall (apply 'nix-funcall (cdr exp)))
    (:attrs (nix-attrs (cdr exp)))
    (:symbol (nix-symbol (cdr exp)))))

(defun nix-list (things)
  (format nil "[ ~{~A~^ ~} ]" (mapcar 'nix-eval things)))

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
  (loop for project in *projects*
        if (string= (getf project :system) system)
          do (return project)))

(defun find-systems (asd)
  "find the entry for asdf system `system-file`"
  (loop for project in *projects*
        when (and (string= (getf project :asd) asd)
                  ;; ignore "special' systems per asdf docs
                  (not (str:contains? "/" (getf project :system))))
          collect (getf project :system)))

(defun find-release (project)
  "find the release entry for project `project-name`"
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
                         for sys = (third (str:words line))
                         when (not (str:contains? "/" sys))
                         collect sys)
                   :test #'string=))


(defun find-system (system-name)
  (loop for project in *projects*
        when (string= (getf project :system) system-name)
          do (return project)))

(defun find-asd (system)
  "Find the asd where this system belongs - that asd can be put in lispLibs"
  (getf (find-system system) :asd))

(defun parse-systems ()
  (loop for system in *systems*
        ;; No need to merge dependencies: a system is the unit of granularity, not the '.asd'

        ;; If the asd is not equal to the system name, we need to make
        ;; a notice that an asd will have to be created

        ;; Only in case no other system depends on this system, we can
        ;; later merge all systems belonging to one project to a
        ;; single package

        for asd = (find-asd system)

        ;; FIXME the optimization from above comment
        ;; for outside-referrers (find-outside-referrers system)

        ;; pass the name of the asd to be copied
        ;; gonna have to think about if should remove system
        ;; declarations from the created asd
        ;; e.g. is asdf gonna try to buiild system 'asd' from
        ;; 'system.asd' later on?
        ;; i.e., scenario: asdf hits `system` in `system.asd`, but
        ;; `system` depends on `asd`, so asdf tries to build it from
        ;; the declaration `system.asd` instead of from `asd.asd`
        ;; Does this happen? XXX: check
        for create-asd? = (when (not (string= asd system)) asd)

        ;; gotta use hash table instead of plist...
        for project = (find-project system)
        for release = (find-release (getf project :project))

        for deps = (getf project :deps)

        for systems = (list system)

        collect `(:asd ,(if create-asd? system asd)
                  :systems ,systems

                  ;; not needed if we create the missing asds
                  :deps ,deps
                  :create-asd? ,create-asd?

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
         for systems = (getf pkg :systems)
         for deps = (getf pkg :deps)
         for url = (getf pkg :url)
         for createAsd = (getf pkg :create-asd?)
         for sha256 = (nix-prefetch-tarball url)
         for version = (getf pkg :version)

         ;; join deps of all `systems`
         for alldeps
           = (loop with all = deps
                   for sys in systems
                   do (setf all (union all (getf (find-system sys) :deps) :test 'string=))
                   finally (return all))

         collect `(,asd
                   . (:attrs
                      . (("pname" . (:string . ,asd))
                         ("createAsd" . ,(if createAsd `(:string . ,createAsd) `(:symbol . "false")))
                         ("version" . (:string . ,version))
                         ("src" . (:funcall . ("createAsd" ((:attrs
                                                                . (("url" . (:string . ,url))
                                                                   ("sha256" . (:string . ,sha256))
                                                                   ("system" . (:string . ,asd))
                                                                   ("asd" . (:string . ,(or createAsd asd)))))))))
                         ("systems" . (:list . ,(mapcar (lambda (sys) `(:string . ,sys)) systems)))
                         ("lispLibs" . (:list . ,(mapcar (lambda (dep) `(:symbol . ,dep))
                                                         (remove-if (lambda (dep) (or (string= dep "asdf")
                                                                                      (string= dep asd))) alldeps))))))))))


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
