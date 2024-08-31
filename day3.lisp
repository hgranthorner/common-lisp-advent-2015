(defpackage :day3
  (:use :cl :alexandria-2))

(in-package :day3)

(defstruct coordinate
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun move (pos c)
  (declare (type coordinate pos))
  (declare (type (standard-char) c))
  (let ((x (coordinate-x pos))
        (y (coordinate-y pos)))
    (case c
      (#\^ (make-coordinate :y (1+ y) :x x))
      (#\v (make-coordinate :y (1- y) :x x))
      (#\> (make-coordinate :y y :x (1+ x)))
      (#\< (make-coordinate :y y :x (1- x)))
      (otherwise nil))))

(defun solve-first ()
  (let
      ((set (make-hash-table :test 'equalp)))
    (loop
      for c across (uiop:read-file-string "inputs/day3.txt")
      for pos = (make-coordinate) then (move pos c)
      do (setf (gethash pos set) t))
    (hash-table-count set)))


(defun with-robot (input)
  (let*
      ((set (make-hash-table :test 'equalp)))
    (setf (gethash (make-coordinate) set) t)
    (loop
      for c across input
      for i = 0 then (1+ i)
      ;; Weird case here - santa needs to move first on the first case before being added
      for santa = (move (make-coordinate) c)
        then (if (evenp i)
                 (move santa c)
                 santa)
      for robot = (make-coordinate)
        then (if (oddp i)
                 (move robot c)
                 robot)
      do (setf (gethash santa set) t)
      do (setf (gethash robot set) t))
    set))

(defun solve-second ()
  (with-robot (str:trim (uiop:read-file-string "inputs/day3.txt"))))
