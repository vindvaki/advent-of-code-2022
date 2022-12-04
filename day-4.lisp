(defpackage :advent-of-code-2022/day-4
  (:use :cl)
  (:import-from #:serapeum
                #:drop
                #:take
                #:lines
                #:~>)
  (:import-from #:uiop
                #:read-file-string)
  (:export
   :*example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-4)


(defparameter *example*
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defun split-middle (list)
  (let* ((length (length list))
         (first-half (ceiling length 2)))
    (list
      (take first-half list)
      (drop first-half list))))

(defun parse-interval-pair (string)
  (~> (ppcre:split "[-,]" string)
      (mapcar #'parse-integer _)
      (split-middle _)))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-interval-pair _)))

(defun fully-contains-p (self other)
  "Does `self' fully contain `other'?"
  (destructuring-bind (a b) self
    (destructuring-bind (c d) other
      (and (<= a c d)
           (>= b d c)))))

(defun should-reconsider-pair-p (interval-pair)
  (destructuring-bind (first second) interval-pair
    (or (fully-contains-p first second)
        (fully-contains-p second first))))

(defun load-input ()
  (read-file-string "day-4.input"))

(defun part-1 (input)
  (~> (parse-input input)
      (count-if #'should-reconsider-pair-p _)))

(defun overlap-p (first second)
  (destructuring-bind (a b) first
    (destructuring-bind (c d) second
      (and
       (>= b c)
       (<= a d)))))

(defun interval-pair-overlap-p (interval-pair)
 (destructuring-bind (first second) interval-pair
   (overlap-p first second)))

(defun part-2 (input)
  (~> (parse-input input)
      (count-if #'interval-pair-overlap-p _)))
