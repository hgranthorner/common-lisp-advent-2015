(defpackage :day13
  (:use :cl :alexandria-2 :alexandria))
(in-package :day13)

(defparameter *sample* "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.")

(defun parse-input (s)
  (declare (type string s))
  (let ((tbl (make-hash-table :test 'equal)))
    (loop for line in (str:lines s)
          for cleaned = (str:trim (str:remove-punctuation line :replacement " "))
          do (destructuring-bind (src _ pos-neg amt _ _ _ _ _ _ dest) (str:words cleaned)
               (declare (ignore _))
               (setf (gethash src tbl)
                     (cons
                      (cons dest (if (string= pos-neg "gain")
                                     (parse-integer amt)
                                     (- (parse-integer amt))))
                      (gethash src tbl)))))
    tbl))

(defun generate-orders (tbl)
  (declare (type hash-table tbl))
  (let ((lst nil))
    (map-permutations
     (lambda (x) (setf lst (cons x lst)))
     (hash-table-keys tbl))
    lst))

(defun solve-first ()
  (let* ((tbl (parse-input (uiop:read-file-string "inputs/day13.txt")))
         (orders (generate-orders tbl)))
    (apply #'max
     (mapcar
      (lambda (order)
        (loop with num-order = (length order)
              for i from 0
              for src in order
              for prev = (if (= i 0)
                             (1- num-order)
                             (1- i))
              for next = (if (= i (1- num-order))
                             0
                             (1+ i))
              for preferences = (gethash src tbl)
              for prev-pref = (cdr (assoc (nth prev order) preferences :test 'equal))
              for next-pref = (cdr (assoc (nth next order) preferences :test 'equal))
              sum (+ prev-pref next-pref)))
      orders))))

(defun add-you-to-tbl (tbl)
  (mapcar
   (lambda (person)
       (setf (gethash person tbl) (cons (cons "you" 0) (gethash person tbl))))
   (hash-table-keys tbl))
  (setf
   (gethash "you" tbl)
   (mapcar (lambda (x) (cons x 0))
           (remove-if (lambda (s) (string= s "you")) (hash-table-keys tbl))))
  tbl)

(defun get-happiness (tbl order)
  (loop with num-order = (length order)
        for i from 0
        for src in order
        for prev = (if (= i 0)
                       (1- num-order)
                       (1- i))
        for next = (if (= i (1- num-order))
                       0
                       (1+ i))
        for preferences = (gethash src tbl)
        for prev-pref = (cdr (assoc (nth prev order) preferences :test 'equal))
        for next-pref = (cdr (assoc (nth next order) preferences :test 'equal))
        sum (+ prev-pref next-pref)))

(defun solve-second ()
  (let* ((tbl (parse-input (uiop:read-file-string "inputs/day13.txt")))
         (tbl (add-you-to-tbl tbl))
         (orders (generate-orders tbl)))
    (loop for happiness in (mapcar (lambda (o) (get-happiness tbl o)) orders)
          maximize happiness)))
