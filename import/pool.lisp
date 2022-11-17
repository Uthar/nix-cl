(defpackage org.lispbuilds.nix/pool
  (:use :cl)
  (:export
   :future
   :thread-pool
   :deref
   :resolve
   :submit
   :invoke-all))

(in-package org.lispbuilds.nix/pool)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-concurrency))

;; Futures

(defclass future ()
  ((value :initform nil)
   (resolvedp :initform nil)
   (lock :initform (sb-thread:make-mutex))
   (cv :initform (sb-thread:make-waitqueue))))

(defmethod deref ((future future))
  (sb-thread:with-mutex ((slot-value future 'lock))
    (or (slot-value future 'resolvedp)
        (sb-thread:condition-wait (slot-value future 'cv)
                                  (slot-value future 'lock)))
    (values (slot-value future 'value)
            (slot-value future 'resolvedp))))

(defmethod resolve ((future future) value)
  (sb-thread:with-mutex ((slot-value future 'lock))
    (when (slot-value future 'resolvedp)
      (error "Future is already resolved."))
    (setf (slot-value future 'value) value
          (slot-value future 'resolvedp) t)
    (sb-thread:condition-notify (slot-value future 'cv))))

(defclass job ()
  ((fn :initarg :fn :initform (error "function required"))
   (args :initarg :args :initform (list))))

(defmethod run-job ((job job))
  (apply (slot-value job 'fn) (slot-value job 'args)))

(defun make-worker (pool)
  (sb-thread:make-thread
   (lambda ()
     (loop (handler-case
               (run-job (sb-concurrency:receive-message
                         (slot-value pool 'queue)))
             (error (e)
               (warn "In pool worker: ~A" e)))))
   :name (format nil "Pool worker ~A"
                 (sb-ext:atomic-incf (car (slot-value pool 'worker-count))))))

(declaim (type fixnum pool-count))
(defglobal pool-count 0)

(defclass thread-pool ()
  ((threads :initform (vector))
   (queue :initform (sb-concurrency:make-mailbox))
   (shutdownp :initform nil)
   (id :initform (sb-ext:atomic-incf pool-count))
   (worker-count :initform (list 0)))
  (:default-initargs
   :size 1))

(defmethod initialize-instance :after ((pool thread-pool) &key size)
  (setf (slot-value pool 'shutdownp) nil)
  (setf (slot-value pool 'threads) (make-array size))
  (dotimes (n size)
    (setf (svref (slot-value pool 'threads) n) (make-worker pool))))

(defmethod shutdown ((pool thread-pool))
  (sb-ext:atomic-update (slot-value pool 'shutdownp) (constantly t))
  (loop for thread across (slot-value pool 'threads)
        do (sb-concurrency:send-message
            (slot-value pool 'queue)
            (lambda ()
              (format t "Killing thread ~A~%" sb-thread:*current-thread*)
              (sb-thread:return-from-thread (values)))))
  (loop for thread across (slot-value pool 'threads)
        do (sb-thread:join-thread thread)))

(defmethod submit ((pool thread-pool) (job job))
  (when (slot-value pool 'shutdownp)
    (error "Pool has shut down."))
  (let* ((future (make-instance 'future))
         (job2 (make-instance 'job
                              :fn (lambda (j)
                                    (resolve future (run-job j)))
                              :args (list job))))
    (sb-concurrency:send-message (slot-value pool 'queue) job2)
    future))

(defun functionsp (sequence)
  (every #'functionp sequence))

(deftype functions ()
  '(and sequence (satisfies functionsp)))

(defmethod invoke-all ((pool thread-pool) (jobs sequence))
  (check-type jobs functions)
  (map 'list (lambda (x) (submit pool x)) jobs))

;; (defparameter pool (make-instance 'thread-pool :size 5))

;; (defparameter future (submit pool (lambda () (sleep 5) (random 100))))

;; (deref future)

;; (shutdown pool)
