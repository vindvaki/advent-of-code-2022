(defpackage :advent-of-code-2022/day-1
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:~>
                #:firstn
                #:lines)
  (:import-from #:alexandria
                #:curry))

(in-package :advent-of-code-2022/day-1)

(defparameter *example* "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defun load-input ()
  (read-file-string "day-1.input"))

(defun safe-parse-integer (string)
  (parse-integer string :junk-allowed t))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'safe-parse-integer _)
      (serapeum:split-sequence nil _)))

(defun part-1 (input)
  (~> (parse-input input)
      (mapcar (curry #'reduce #'+) _)
      (reduce #'max _)))

(defun part-2 (input)
  (~> (parse-input input)
      (mapcar (curry #'reduce #'+) _)
      (sort _ #'>=)
      (firstn 3 _)
      (reduce #'+ _)))
