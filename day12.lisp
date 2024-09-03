(defpackage :day12
  (:use :cl :alexandria-2))
(in-package :day12)



(defun simplify (x)
  (typecase x
    (hash-table (cons :table (hash-table-plist x)))
    (t x)))

(let ((tbl (make-hash-table :test 'equal)))
  (setf (gethash "abc" tbl) 123)
  (hash-table-plist tbl))

(defun get-numbers (xs)
  (let ((xs (simplify xs)))
    (typecase xs
      (integer xs)
      (list (loop for x in xs
                  sum (get-numbers x)))
      (t 0))))

(defun solve-first ()
  (get-numbers
   (yason:parse
    (uiop:read-file-string "inputs/day12.json"))))

(defun get-numbers-2 (xs)
  (let ((xs (simplify xs)))
    (typecase xs
      (integer xs)
      (list (if (and (equal :table (car xs))
                     (find "red" xs :test 'equal))
                0
                (loop for x in xs
                      sum (get-numbers-2 x))))
      (t 0))))

(defun solve-second ()
  (get-numbers-2
   (yason:parse
    (uiop:read-file-string "inputs/day12.json"))))
