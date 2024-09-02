(defpackage :day11
  (:use :cl :alexandria-2))
(in-package :day11)

(defparameter *current-password* (coerce "hepxcrrq" 'list))

(defun char+ (c)
  (code-char (1+ (char-code c))))

(defun chunk-into-triples (lst)
  (let ((xs (if (listp lst) lst
                (coerce lst 'list))))
    (loop for (a b c) on xs
          while c
          collect (list a b c))))

(defun increment-password (pw)
  (let* ((i (1- (length pw)))
         (c (nth i pw)))
    (if (char= c #\z)
        (progn
          (setf (nth i pw) #\a)
          (loop with index = (1- i)
                for i-c = (nth index pw)
                if (char= i-c #\z)
                  do (progn
                       (setf (nth index pw) #\a)
                       (setf index (1- index)))
                else
                  return (setf (nth index pw) (char+ i-c))
                end)
          pw)
        (progn
          (setf (nth i pw) (char+ c))
          pw))))

(defun has-straight-p (pw)
  (handler-case
      (destructuring-bind (a b c &rest rest) pw
        (declare (ignore rest))
        (if (char= (char+ (char+ a)) (char+ b) c)
            t
            (has-straight-p (cdr pw))))
    (error () nil)))

(defun no-bad-letters-p (pw)
  (not (find-if (lambda (c) (or (char= c #\i)
                                (char= c #\o)
                                (char= c #\l)))
                pw)))

(defun two-pairs-p (pw)
  (loop with count = 0
        with in-triple = #\1
        for (a b c) in (chunk-into-triples pw)
        if (and (or (char= a b) (char= b c))
                (not (char= a c))
                (not (char= b in-triple)))
          do (progn
               (setf in-triple b)
               (setf count (1+ count)))
        else if (not (char= b in-triple))
          do (setf in-triple #\1)
        end
        if (<= 2 count)
          return t
        end))

(defun valid-p (pw)
  (and (has-straight-p pw)
       (no-bad-letters-p pw)
       (two-pairs-p pw)))

(defun solve-first (pw)
  (coerce
   (loop with pw = (increment-password (copy-list pw))
         if (not (valid-p pw))
           do (setf pw (increment-password pw))
         else
           return pw
         end)
   'string))

(defun solve-second ()
  (let ((first-valid (solve-first (coerce *current-password* 'list))))
    (solve-first (coerce first-valid 'list))))
