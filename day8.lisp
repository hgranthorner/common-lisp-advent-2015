(defpackage :day8
  (:use :cl :alexandria-2))
(in-package :day8)

(defparameter *example* (caddr (uiop:read-file-lines "inputs/day8.txt")))
(defparameter *problem* "\"hey\\\\\"")

(defun str-replace (old new s &key regex)
  (multiple-value-bind (s _) (str:replace-all old new s :regex regex)
    (declare (ignore _))
    s))

(defun count-line (s &key debug)
  (declare (type string s))
  (let* ((s (str-replace "\\\"" "\"" s))
         (s (str-replace "\\\\" "\\" s))
         (s (str:replace-all "\\\\x[a-f0-9]{2}" "z" s :regex t)))
    (if debug
        s
        (max 0 (- (length s) 2)))))

(defun solve-first (file)
  (loop for line in (uiop:read-file-lines file)
        for trimmed = (str:trim line)
        sum (- (length trimmed) (count-line trimmed))))

(defun expand-line (s &key debug)
  (declare (type string s))
  (let* ((s (str-replace "\\" "\\\\" s))
         (s (str-replace "\"" "\\\"" s :regex t))
         (s (str-replace "\\x[a-f0-9]{2}" "\\\\xzz" s :regex t)))
    (if debug
        s
        (+ 2 (length s)))))

(defun solve-second ()
  (loop for line in (uiop:read-file-lines "inputs/day8.txt")
        for expanded = (expand-line line)
        sum (- expanded (length line))))
