(defpackage :advent-of-code-2022/day-5
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:drop
                #:take
                #:lines)
  (:import-from #:ppcre
                #:split)
  (:export :*example*
           :load-input
           :part-1
           :part-2))

(in-package :advent-of-code-2022/day-5)

(defparameter *example*
  (read-file-string "day-5.example"))

(defun load-input ()
  (read-file-string "day-5.input"))

(defun parse-stacks (string)
  (loop with lines = (cdr (reverse (lines string)))
        with count = (ceiling (length (car lines)) 4)
        with stacks = (make-array count :initial-element nil :element-type 'list)
        for line in lines do
          (loop for i from 0 to (1- count)
                for j = (1+ (* 4 i))
                for c = (aref line j)
                unless (char= #\Space c) do
                  (push c (aref stacks i)))
        finally (return stacks)))

(defun parse-instruction-line (string)
  (ppcre:register-groups-bind ((#'parse-integer count) (#'parse-integer from) (#'parse-integer to))
      ("move (\\d+) from (\\d) to (\\d)" string)
    (list count
          (1- from)
          (1- to))))

(defun parse-input (input)
  (destructuring-bind (stacks-string instructions-string) (split "\\n\\n" input)
    (list
      (parse-stacks stacks-string)
      (mapcar #'parse-instruction-line (lines instructions-string)))))

(defun part-1 (input)
  (destructuring-bind (stacks instructions) (parse-input input)
    (loop for (count from-idx to-idx) in instructions do
          (loop for i upto (1- count) do
                (push (pop (aref stacks from-idx))
                      (aref stacks to-idx)))
          finally (return (map 'string #'car stacks)))))

(defun part-2 (input)
  (destructuring-bind (stacks instructions) (parse-input input)
    (loop for (count from-idx to-idx) in instructions
          for buf = (take count (aref stacks from-idx)) do
            (setf (aref stacks from-idx) (drop count (aref stacks from-idx))
                  (aref stacks to-idx) (nconc buf (aref stacks to-idx)))
          finally (return (map 'string #'car stacks)))))
