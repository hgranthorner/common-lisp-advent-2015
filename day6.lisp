(defpackage :day6
  (:use :cl :alexandria-2))
(in-package :day6)

(defstruct (command (:conc-name cmd-))
  (type :toggle :type keyword)
  (start '(0 0) :type list)
  (end '(0 0) :type list))

(declaim (type (function (string) command) parse-line))
(defun parse-line (s)
  (declare (type (string) s))
  (let* ((split (str:split " " (str:trim s)))
         (cmd (cond ((string= (car split) "toggle") :toggle)
                    ((string= (cadr split) "on") :turn-on)
                    ((string= (cadr split) "off") :turn-off)))
         (start-str (if (eql cmd :toggle)
                        (cadr split)
                        (caddr split)))
         (end-str (if (eql cmd :toggle)
                        (cadddr split)
                        (car (last split))))
         (start (mapcar #'parse-integer (str:split "," start-str)))
         (end (mapcar #'parse-integer (str:split "," end-str))))
    (make-command :type cmd :start start :end end)))

(declaim (type (function (command array)) execute-command))
(defun execute-command (cmd arr)
  (loop for x from (car (cmd-start cmd)) to (car (cmd-end cmd))
        do (loop
             for y from (cadr (cmd-start cmd)) to (cadr (cmd-end cmd))
             do (setf (aref arr x y)
                      (case (cmd-type cmd)
                        (:toggle (not (aref arr x y)))
                        (:turn-on t)
                        (:turn-off nil))))))

(defun solve-first ()
  (let ((arr (make-array '(1000 1000) :initial-element nil)))
    (loop for line in (uiop:read-file-lines "inputs/day6.txt")
          for cmd = (parse-line line)
          do (execute-command cmd arr))
    (destructuring-bind (n m) (array-dimensions arr)
      (loop for i from 0 below n
            sum (loop for j from 0 below m
                      count (aref arr i j))))))

(declaim (type (function (command array)) execute-command-2))
(defun execute-command-2 (cmd arr)
  (loop for x from (car (cmd-start cmd)) to (car (cmd-end cmd))
        do (loop
             for y from (cadr (cmd-start cmd)) to (cadr (cmd-end cmd))
             do (setf (aref arr x y)
                      (case (cmd-type cmd)
                        (:toggle (+ 2 (aref arr x y)))
                        (:turn-on (1+ (aref arr x y)))
                        (:turn-off (if (= 0 (aref arr x y))
                                       0
                                       (- (aref arr x y) 1))))))))


(defun solve-second ()
  (let ((arr (make-array '(1000 1000) :initial-element 0)))
    (loop for line in (uiop:read-file-lines "inputs/day6.txt")
          for cmd = (parse-line line)
          do (execute-command-2 cmd arr))
    (destructuring-bind (n m) (array-dimensions arr)
      (loop for i from 0 below n
            sum (loop for j from 0 below m
                      sum (aref arr i j))))))
