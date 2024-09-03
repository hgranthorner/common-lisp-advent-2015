(defpackage :day9
  (:use :cl :alexandria-2))
(in-package :day9)

(defparameter *lines* (uiop:read-file-lines "inputs/day9.txt"))
(defparameter *example-lines* (str:lines "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"))

(defstruct trip start end length)

(defun parse-line (s)
  (declare (type string s))
  (destructuring-bind (start _to end _= distance) (str:words (str:trim s))
    (declare (ignore _to _=))
    (cons (make-trip :start end :end start :length distance)
          (make-trip :start start :end end :length distance))))

(defun all-locations (trips)
  (remove-duplicates (append (mapcar #'trip-start trips) (mapcar #'trip-end trips)) :test 'equal))

;; get all the trips from the starting location
(defun get-possible-trips (trips traveled starting-location)
  (remove-if (lambda (trip) (or (find (trip-end trip) traveled :test 'equal)
                                (not (string= (trip-start trip) starting-location))))
             trips))

(defun find-shortest-path (trips traveled)
  (loop for trip in trips
        for next-trips = (get-possible-trips trips traveled trip)))

(defun find-all-paths (trips traveled location)
  (declare (type list trips))
  (declare (type list traveled))
  (declare (type string location))
  (let* ((next-trips (get-possible-trips trips traveled location))
         (now-traveled (cons location traveled)))
    (print next-trips)
    (loop for trip in next-trips
          collect (cons trip (find-all-paths trips now-traveled (trip-end trip))))))

(defun find-all-trips (trips)
  (let* ((locations (all-locations trips)))
    (loop for location in locations
          append (find-all-paths trips nil location))))

(defun calc-all-trips (trip-tree total-so-far)
  (loop for trip-branch in trip-tree
        for distance = (parse-integer (trip-length (car trip-branch)))
        if (not (cdr trip-branch))
          return (+ distance total-so-far)
        end
        collect (calc-all-trips (cdr trip-branch) (+ total-so-far distance))))

(defun solve-first ()
  (apply #'min (flatten (calc-all-trips (find-all-trips (flatten (mapcar #'parse-line *lines*))) 0))))

(defun solve-second ()
  (apply #'max (flatten (calc-all-trips (find-all-trips (flatten (mapcar #'parse-line *lines*))) 0))))
