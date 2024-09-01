(defpackage :day7
  (:use :cl :alexandria-2))
(in-package :day7)

(defun rash (integer count)
  (ash integer (- count)))

(defparameter *ops*
  (list
   (cons "LSHIFT" (lambda (x y) (integer-to-bit-array (ash (bit-array-to-integer x) (bit-array-to-integer y)))))
   (cons "RSHIFT" (lambda (x y) (integer-to-bit-array (rash (bit-array-to-integer x) (bit-array-to-integer y)))))
   (cons "AND" (lambda (x y) (bit-and (integer-to-bit-array x) (integer-to-bit-array y))))
   (cons "OR" (lambda (x y) (bit-ior (integer-to-bit-array x) (integer-to-bit-array y))))
   (cons "NOT" (lambda (x) (bit-not (integer-to-bit-array x))))))

;; note - we always use u16s
(defun integer-to-bit-array (n)
  (if (simple-bit-vector-p n) n
      (let* ((num-bits (integer-length n))
             (bit-array (make-array num-bits :element-type 'bit))
             (u16-bit-array (make-array 16 :element-type 'bit)))
        (loop for i from 0 below num-bits
              do (setf (aref bit-array (- num-bits i 1)) (ldb (byte 1 i) n)))
        (loop for i from 0 below num-bits
              for u16-i = (+ (- 16 num-bits) i)
              do (setf (aref u16-bit-array u16-i) (aref bit-array i)))
        u16-bit-array)))

(defun bit-array-to-integer (bit-array)
  (if (integerp bit-array) bit-array
      (let ((n 0))
        (loop for bit across bit-array
              do (setf n (+ (* n 2) bit)))
        n)))

;; Source can be:
;;   (:value str-value|integer-value)
;;   (:unary "command" value)
;;   (:binary "command" lvalue rvalue)
(defstruct wire source dest)

(defun parse-line (line)
  (declare (type (string) line))
  (let* ((split (str:split " -> " (str:trim line)))
         (lsplit (str:split " " (car split)))
         (dest (cadr split))
         (source (case (length lsplit)
                   (1 (list :value (car lsplit)))
                   (2 (cons :unary lsplit))
                   (3 (list :binary (cadr lsplit) (car lsplit) (caddr lsplit))))))
    (make-wire :source source :dest dest)))
(parse-line "x AND y -> e")

(defun calc-value (wires table s)
  (declare (type list wires))
  (declare (type hash-table table))
  (declare (type string s))
  (let* ((prev-wire (find-if
                     (lambda (wire) (string= (wire-dest wire) s))
                     wires))
         (prev-source (wire-source prev-wire)))
    (integer-to-bit-array
     (case (car prev-source)
       (:value (destructuring-bind (_ value) prev-source
                 (declare (ignore _))
                 (handler-case (parse-integer value)
                   (error () (get-value wires table value)))))
       (:unary (destructuring-bind (_ cmd value) prev-source
                 (declare (ignore _))
                 (let ((f (cdr (assoc cmd *ops* :test 'string=))))
                   (funcall f (get-value wires table value)))))
       (:binary (destructuring-bind (_ cmd v1 v2) prev-source
                  (declare (ignore _))
                  (let ((f (cdr (assoc cmd *ops* :test 'string=))))
                    (funcall f (get-value wires table v1)
                             (get-value wires table v2)))))))))

(defun get-value (wires table s)
  (declare (type (string) s))
  (handler-case (parse-integer s)
    (error ()
      (if-let ((val (gethash s table)))
        val
        (let ((val (calc-value wires table s)))
          (setf (gethash s table) val)
          val)))))

(defun solve-first ()
  (let* ((wires (loop for line in (uiop:read-file-lines "inputs/day7.txt")
                      collect (parse-line (str:trim line))))
         (table (make-hash-table :test 'equal)))
    (bit-array-to-integer (get-value wires table "a"))))

(defun solve-second ()
  (let* ((wires (loop for line in (uiop:read-file-lines "inputs/day7.txt")
                      collect (parse-line (str:trim line))))
         (table (make-hash-table :test 'equal))
         (original-a (bit-array-to-integer (get-value wires table "a")))
         (table (make-hash-table :test 'equal)))
    (setf (gethash "b" table) original-a)
    (bit-array-to-integer (get-value wires table "a"))))
