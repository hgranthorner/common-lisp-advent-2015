(defpackage :day5
  (:use :cl :alexandria-2))
(in-package :day5)

"
A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
"

(defun three-vowels-p (s)
  (let ((arr (make-array 5 :initial-element 0)))
    (loop for c across s
          do (case c
               (#\a (setf (aref arr 0) (1+ (elt arr 0))))
               (#\e (setf (aref arr 1) (1+ (elt arr 1))))
               (#\i (setf (aref arr 2) (1+ (elt arr 2))))
               (#\o (setf (aref arr 3) (1+ (elt arr 3))))
               (#\u (setf (aref arr 4) (1+ (elt arr 4))))
               (otherwise nil)))
    (<= 3 (loop for n across arr
                sum n))))

(three-vowels-p "aei")
(three-vowels-p "xazegov")
(three-vowels-p "aeiouaeiouaeiou")
(three-vowels-p "abc")

(defun twice-in-a-row-p (s)
  (loop
    for prevc = nil then c
    for c across s
    when (equal prevc c)
      return t
    end))

(twice-in-a-row-p "abc")
(twice-in-a-row-p "abbc")

(defun in-set-p (s)
  (loop
    for i below (1- (length s))
    for sub = (str:substring i (+ 2 i) s)
    when (or
          (string= "ab" sub) (string= "cd" sub) (string= "pq" sub) (string= "xy" sub))
      return t
    end))

(defun nice-p (s)
  (and (three-vowels-p s)
       (twice-in-a-row-p s)
       (not (in-set-p s))))

(equal (nice-p "ugknbfddgicrmopn") t)
(equal (nice-p "jchzalrnumimnmhp") nil)
(equal (nice-p "haegwjzuvuyypxyu") nil)
(equal (nice-p "dvszwmarrgswjxmb") nil)

(defun solve-first ()
  (loop for line in (uiop:read-file-lines "inputs/day5.txt")
        count (nice-p (str:trim line))))

"
It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
"

(defun chunk-into-pairs (lst)
  (let ((xs (if (listp lst) lst
                (coerce lst 'list))))
    (loop for (a b) on xs
          while b
          collect (list a b))))

(defun chunk-into-triples (lst)
  (let ((xs (if (listp lst) lst
                (coerce lst 'list))))
    (loop for (a b c) on xs
          while c
          collect (list a b c))))

(defun double-pair-p (s)
  (declare (type (string) s))
  (let
      ((freqs (make-hash-table :test 'equal)))
    (loop
      with prev-pair = nil
      for pair in (chunk-into-pairs s)
      unless (and prev-pair (equal prev-pair pair))
        do (let ((current-val (or (gethash pair freqs) 0)))
             (setf (gethash pair freqs) (1+ current-val))
             (setf prev-pair pair))
      end)
    (loop for val being the hash-values in freqs
          if (< 1 val)
            return t
          end)))

(double-pair-p "xyxy")
(double-pair-p "aabcdefgaa")
(double-pair-p "aaa")

(defun same-around-p (s)
  (declare (type (string) s))
  (loop
    for (a b c) in (chunk-into-triples s)
    if (char= a c)
      return t
    end))

(same-around-p "abc")
(same-around-p "aba")
(same-around-p "taba")
(same-around-p "aaa")

(defun nice-2-p (s &key debug)
  (let* ((trimmed (str:trim s))
         (double-pair (double-pair-p trimmed))
         (same-around (same-around-p trimmed))
         (conds (list double-pair same-around)))
    (if debug
        (switch (conds :test 'equal)
          ((list t t) t)
          ((list nil t) :same-around)
          ((list t nil) :double-pair)
          ((list nil nil) :neither))
    (and double-pair same-around))))

(nice-2-p "qjhvhtzxzqqjkmpb" :debug t)
(nice-2-p "xxyxx")
(nice-2-p "uurcxstgmygtbstg")
(nice-2-p "ieodomkazucvgmuy")

(loop for s in (uiop:read-file-lines "inputs/day5.txt")
      for trimmed = (str:trim s)
      collect (list s (nice-2-p trimmed :debug t)))

(defun solve-second ()
  (loop for line in (uiop:read-file-lines "inputs/day5.txt")
        for trimmed = (str:trim line)
        count (and (double-pair-p trimmed) (same-around-p trimmed))))
