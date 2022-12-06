(defpackage :advent-of-code-2022/day-6
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:drop
                #:take)
  (:export :load-input
           :part-1
           :part-2))

(in-package :advent-of-code-2022/day-6)

(defun load-input ()
  (read-file-string "day-6.input"))

(defun part-1 (input)
  (find-distinct-run input 4))

(defun part-2 (input)
  (find-distinct-run input 14))

(defun find-distinct-run (string length)
  (do* ((i 0 (1+ i))
        (table (let ((table (make-hash-table)))
                 (dotimes (k length table)
                   (incf (gethash (aref string k) table 0))))))
      ((= length (hash-table-count table)) (+ i length))
    (decf (gethash (aref string i) table))
    (when (= 0 (gethash (aref string i) table))
      (remhash (aref string i) table))
    (incf (gethash (aref string (+ i length)) table 0))))
