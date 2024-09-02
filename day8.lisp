(defpackage :day8
  (:use :cl :alexandria-2))
(in-package :day8)

(defparameter *example* (caddr (uiop:read-file-lines "inputs/day8.txt")))
(defparameter *problem* "\"hey\\\\\"")

(defun count-line (s &key debug)
  (declare (type string s))
    (multiple-value-bind (s _) (str:replace-all "\\\"" "\"" s)
    (declare (ignore _))
      (multiple-value-bind (s _) (str:replace-all "\\\\" "\\" s)
      (declare (ignore _))
      (multiple-value-bind (s _) (str:replace-all "\\\\x[a-f0-9]{2}" "z" s :regex t)
        (declare (ignore _))
        (if debug
            s
            (max 0 (- (length s) 2)))))))

(defun solve-first (file)
  (loop for line in (uiop:read-file-lines file)
        for trimmed = (str:trim line)
        sum (- (length trimmed) (count-line trimmed))))
