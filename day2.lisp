(defpackage :day2
  (:use :cl :alexandria-2))

(in-package :day2)

(defun mapcar-combinations (f xs &key length)
  (let ((out nil))
    (map-combinations (lambda (x) (setf out (cons (funcall f x) out)))
                      xs
                      :length length)
    out))

(defun areas (l w h)
  (mapcar-combinations (lambda (x) (* (car x) (cadr x)))
                       (list l w h)
                       :length 2))

(defun parse-input (input)
  (destructuring-bind (l w h) (mapcar #'parse-integer (str:split "x" input))
    (values l w h)))

(defun wrapping-paper (input)
  (declare (type (string) input))
  (multiple-value-bind (l w h) (parse-input input)
    (let* ((areas (areas l w h))
           (smallest (apply #' min areas)))
      (+ (* 2 l w)
         (* 2 l h)
         (* 2 h w)
         smallest))))

(defun solve-first ()
  (loop
    for line in (uiop:read-file-lines "inputs/day2.txt")
    sum (wrapping-paper line)))

(defun ribbon (input)
  (declare (type (string) input))
  (destructuring-bind (s1 s2 s3) (sort (multiple-value-list (parse-input input)) #'<)
    (+ s1 s1 s2 s2 (* s1 s2 s3))))

(defun solve-first ()
  (loop
    for line in (uiop:read-file-lines "inputs/day2.txt")
    sum (wrapping-paper line)))

(defun solve-second ()
  (loop
    for line in (uiop:read-file-lines "inputs/day2.txt")
    sum (ribbon line)))
