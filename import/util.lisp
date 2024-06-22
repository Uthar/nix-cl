(defpackage org.lispbuilds.nix/util
  (:use :cl)
  (:import-from :ppcre)
  (:import-from :sb-concurrency)
  (:export
   :replace-regexes
   :make-count-down-latch
   :count-down
   :await))

(in-package org.lispbuilds.nix/util)

(defun replace-regexes (from to str)
  (assert (= (length from) (length to)))
  (if (null from)
      str
      (replace-regexes
       (rest from)
       (rest to)
       (ppcre:regex-replace-all (first from) str (first to)))))

(defstruct (count-down-latch (:conc-name %latch-))
  (gate (sb-concurrency:make-gate) :type sb-concurrency:gate)
  (count (error "count required") :type (unsigned-byte 64)))

(defun count-down (latch &optional (amount 1))
  (when (<= (sb-ext:atomic-decf (%latch-count latch) amount) amount)
    (sb-concurrency:open-gate (%latch-gate latch))))

(defun await (latch &optional timeout)
  (sb-concurrency:wait-on-gate (%latch-gate latch) :timeout timeout))
  
