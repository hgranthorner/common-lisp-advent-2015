(defpackage :day1
  (:use :cl))

(in-package :day1)

(defparameter examples
  (list
   '("(())" 0)
   '("(((" 3)
   '("(()(()(" 3)
   '("))(((((" 3)
   '("())" -1)
   '("))(" -1)
   '(")))" -3)
   '(")())())" -3)))

(defun change-floor (c)
  (declare (type (standard-char) c))
  (case c
    (#\( 1)
    (#\) -1)
    (otherwise 0)))

(defun solve-first (input)
  (declare (type (string) input))
  (loop for c across input
        sum (change-floor c)))

(defun solve-second (input)
  (declare (type (string) input))
  (loop for c across input
        for i = 1 then (1+ i)
        summing (change-floor c) into floor
        do (print floor)
        when (< floor 0)
          return i
        end
        finally (return nil)))

;; (solve-first (uiop:read-file-string "inputs/day1.txt")) = 74
;; (solve-second (uiop:read-file-string "inputs/day1.txt")) = 1795
