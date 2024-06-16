(in-package :cl-user)
(load "@asdf@")

(format t "SRC(E): ~A~%" (uiop:getenv "CL_SOURCE_REGISTRY"))
(format t "OUT(E): ~A~%" (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS"))

;; (when (string= "flexi-streams" (uiop:getenv "pname"))
;;   (error "bla"))

;; TODO warn about uncompiled slashy systems!!!

;; TODO possibly document how to allow compiling uncompiled slashy systems into
;; "user cache" via an additional runtime output translation configuration.

(defvar *pwd* (namestring (uiop:getcwd)))


(asdf:initialize-source-registry 
 `(:source-registry :inherit-configuration (:tree ,*pwd*)))
(asdf:initialize-output-translations 
 `(:output-translations
   :disable-cache
   ("/nix/store/" "/nix/store/")
   (,*pwd* (,*pwd* "__tmpfasl__"))
   :inherit-configuration))

(defvar *declared-systems* (uiop:split-string (uiop:getenv "systems")))

(defvar *system*)

;; (defmethod asdf/component:around-compile-hook :before ((c asdf:system))
;;   (format t "[INFO] Checking... ~A ~A" c *system*)
;;   (unless (member (asdf:primary-system-name c) *declared-systems* :test #'string=)
;;     (error "Undeclared dependency on downstream slashy system when loading '~A'. Please override '~A' with system '~A'" *system* (asdf:primary-system-name c) (asdf:component-name c))))

;; (defmethod asdf:operate :before (o c &key)
;;   (format t "[INFO] Checking... ~A ~A ~A ~A~%" o c *system* (asdf:output-files o c)))

(defvar *out* (uiop:getenv "out"))

(defun out-path-p (path)
  (or (uiop:string-prefix-p *pwd* (namestring path))
      (uiop:string-prefix-p *out* (namestring path))))

(defmethod asdf:perform :before ((o asdf:compile-op) (c asdf:source-file))
  (let ((out (asdf:output-files o c)))
    (format t "[INFO] [OUT] Checking... ~A ~A ~A ~A~%" o c *system* out)
    ;; (let ((missing (find-if-not #'uiop:probe-file* out)))
    (when (find-if-not #'out-path-p out)
      (error "While loading '~A': Undeclared subsystem dependency on '~A' in '~A'." *system* (asdf:component-name (asdf:component-system c)) (asdf:primary-system-name c)))
    ;; (uiop:ensure-all-directories-exist out)
    ))

;; (defmethod asdf:output-files :around ((o asdf:operation) (c asdf:component))
;;   (loop for file in (call-next-method)
;;         for type = (pathname-type file)
;;         collect (progn
;;                   (format t "[CHKFL]: ~A~%" file)
;;                   (cond
;;                     ((string= "so" type)
;;                      #P"BLAAAAAAA")
;;                     (t file)))))

(format t "SRC: ~A~%" asdf:*source-registry-parameter*)
(format t "OUT: ~A~%" asdf:*output-translations-parameter*)

(defun load-systems ()
  (handler-case
      (dolist (s *declared-systems*)
        (let ((*system* s))
          (asdf:load-system s)))
    (error (c)
      (format t "BUILD FAILED: ~A~%" c)
      (error c))))

(load-systems)

(with-open-file (done ".lisp-build-done" :direction :output)
  (write-line "done" done))
