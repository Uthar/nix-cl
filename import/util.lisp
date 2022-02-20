(defpackage org.nixos.lisp/util
  (:use :cl)
  (:import-from :ppcre)
  (:export
   :replace-regexes))

(in-package org.nixos.lisp/util)

(defun replace-regexes (from to str)
  (assert (= (length from) (length to)))
  (if (null from)
      str
      (replace-regexes
       (rest from)
       (rest to)
       (ppcre:regex-replace-all (first from) str (first to)))))
