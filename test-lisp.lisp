#!/usr/bin/env -S sbcl --script

(require :uiop)

(defparameter packages (uiop:read-file-lines "./lispPackages.txt"))

(defparameter lisp (or (cadr sb-ext:*posix-argv*) "sbcl"))

(defparameter nix-build "nix-build -E 'with import <nix-cl> {}; ~aPackages.~a'")

(defparameter cpu-count
  (length
   (remove-if-not
    (lambda (line)
      (uiop:string-prefix-p "processor" line))
    (uiop:read-file-lines "/proc/cpuinfo"))))

(defparameter sem (sb-thread:make-semaphore :count cpu-count))

(defparameter statuses (make-hash-table :synchronized t))

(setf (uiop:getenv "NIX_PATH")
      (uiop:strcat (uiop:getenv "NIX_PATH")
                   ":nix-cl="
                   (format nil "~a" (uiop:getcwd))))

(defparameter log-lock (sb-thread:make-mutex))

(format *error-output* "Testing ~a~%" lisp)

(defun clear-line ()
  (write-char #\Return *error-output*)
  (write-char #\Escape *error-output*)
  (write-char #\[ *error-output*)
  (write-char #\K *error-output*))

(declaim (type fixnum errors))
(defglobal errors 0)

(dolist (pkg packages)
  (sb-thread:wait-on-semaphore sem)
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (unwind-protect
              (let ((code
                      (setf
                       (gethash pkg statuses)
                       (nth-value 2
                                  (uiop:run-program
                                   (format nil nix-build lisp pkg)
                                   :ignore-error-status t)))))
                (sb-thread:with-mutex (log-lock)
                  (clear-line)
                  (format *error-output* "[~a/~a] ~[OK~:;ERROR~] ~a~[~:;~%~]"
                          (hash-table-count statuses)
                          (length packages)
                          code
                          pkg
                          code)
                  (force-output *error-output*))
                (unless (zerop code)
                  (sb-ext:atomic-incf errors)))
           (sb-thread:signal-semaphore sem))
       (error (e)
         (format t "~a~%" e)
         (sb-ext:quit :recklessly-p t :unix-status 1))))))

(sb-thread:wait-on-semaphore sem :n cpu-count)

(format *error-output* "~%Done (~a/~a)."
        (- (length packages) errors)
        (length packages))

(when (plusp errors)
  (format *error-output* "~%~%~a Errors: " errors)
  (maphash (lambda (k v)
             (unless (zerop v)
               (format *error-output* "~%  ~a" k)))
           statuses))
