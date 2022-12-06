(defpackage :advent-of-code-2022/day-6
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:drop
                #:take))

(in-package :advent-of-code-2022/day-6)

(defun load-input ()
  (read-file-string "day-6.input"))

(defun part-1 (input)
  (find-distinct-run input 4))

(defun part-2 (input)
  (find-distinct-run input 14))

(defun find-distinct-run (string length)
  (do ((i length (1+ i))
       (head (coerce (take length string) 'list))
       (rest (drop length string) (drop 1 rest)))
      ((= length (length (remove-duplicates head))) i)
    (let ((next-head (cdr head)))
      (setf (car head) (elt rest 0)
            (cdr head) nil
            (cdr (last next-head)) head
            head next-head))))
