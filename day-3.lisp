(defpackage :advent-of-code-2022/day-3
  (:use :cl)
  (:import-from #:serapeum
                #:~>
                #:summing
                #:drop
                #:take
                #:lines)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export
   :*example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-3)

(defparameter *example*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defun split-rucksack (seq)
  (let ((n (length seq)))
    (cons
     (subseq seq 0 (floor n 2))
     (subseq seq (floor n 2)))))

(defun item-priority (char)
  (if (upper-case-p char)
      (+ 27 (- (char-code char) (char-code #\A)))
      (+ 1 (- (char-code char) (char-code #\a)))))

(defun part-1 (input)
  (loop for line in (lines input)
        for (first . second) = (split-rucksack line)
        for x = (car (intersection (coerce first 'list) (coerce second 'list)))
        summing (item-priority x)))

(defun load-input ()
  (read-file-string "day-3.input"))


(defmacro do-slices ((slice length list) &body body)
  (with-gensyms (eval-length eval-list rest)
    `(do* ((,eval-list ,list)
           (,eval-length ,length)
           (,slice (take ,eval-length ,eval-list) (take ,eval-length ,rest))
           (,rest (drop ,eval-length ,eval-list) (drop ,eval-length ,rest)))
      ((not ,slice))
      ,@body)))

(defun as-list (obj)
  (coerce obj 'list))

(defun part-2 (input)
  (summing
    (do-slices (slice 3 (lines input))
      (sum (~> (mapcar #'as-list slice)
               (reduce #'intersection _)
               (mapcar #'item-priority _)
               (reduce #'max _))))))
