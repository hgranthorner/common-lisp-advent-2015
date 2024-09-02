(defpackage :day10
  (:use :cl :alexandria-2))
(in-package :day10)

(defun digit-to-char (n)
  (code-char (+ n (char-code #\0))))

(defun compress (s)
  (let ((prev-c nil)
        (collected 0))
    (append (loop
              for c in (coerce s 'list)
              do (when (not prev-c)
                   (setf prev-c c))
              if (char= prev-c c)
                do (setf collected (1+ collected))
              else
                append (let ((res (list (digit-to-char collected) prev-c)))
                         (setf prev-c c)
                         (setf collected 1)
                         res)
              end)
            (list (digit-to-char collected) prev-c))))

(defun solve (n)
  (let ((value "1113222113"))
    (loop repeat n
          do (setf value (compress value)))
    (length value)))

;; (solve 40) <- first
;; (solve 50) <- second
