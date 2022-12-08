(defpackage :advent-of-code-2022/day-8
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:lines
                #:~>)
  (:export
   :*example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-8)

(defparameter *example*
  "30373
25512
65332
33549
35390")

(defun load-input ()
  (read-file-string "day-8.input"))

(defun coerce-to-list (object)
  (coerce object 'list))

(defun digit-from-char (char)
  (- (char-code char) (char-code #\0)))

(defun digit-list-from-char-list (list)
  (mapcar #'digit-from-char list))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'coerce-to-list _)
      (mapcar #'digit-list-from-char-list _)
      (make-array (list (length _) (length (car _)))
                  :element-type 'fixnum
                  :initial-contents _)))

(defun horizon (grid row-0 col-0)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (let ((pivot (aref grid row-0 col-0)))
        (list
          :down (loop for row from (1+ row-0) upto (1- rows)
                      until (<= pivot (aref grid row col-0))
                      finally (return row))
          :up (loop for row from (1- row-0) downto 0
                    until (<= pivot (aref grid row col-0))
                    finally (return row))
          :left (loop for col from (1- col-0) downto 0
                      until (<= pivot (aref grid row-0 col))
                      finally (return col))
          :right (loop for col from (1+ col-0) upto (1- cols)
                       until (<= pivot (aref grid row-0 col))
                       finally (return col))))))

(defun part-1 (input)
  (let ((grid (parse-input input))
        (count 0))
    (destructuring-bind (rows cols) (array-dimensions grid)
      (dotimes (row rows)
        (dotimes (col cols)
          (let ((horizon (horizon grid row col)))
            (when (or (= (getf horizon :up) -1)
                      (= (getf horizon :down) rows)
                      (= (getf horizon :left) -1)
                      (= (getf horizon :right) cols))
              (incf count))))))
    count))

(defun part-2 (input)
  (let ((grid (parse-input input))
        (max-score 0))
    (destructuring-bind (rows cols) (array-dimensions grid)
      (loop for row from 1 upto (- rows 2) do
        (loop for col from 1 upto (- cols 2) do
          (let* ((horizon (horizon grid row col))
                 (score (* (- row (max (getf horizon :up) 0))
                           (- (min (getf horizon :down) (1- rows)) row)
                           (- col (max (getf horizon :left) 0))
                           (- (min (getf horizon :right) (1- cols)) col))))
            (when (> score max-score)
              (setf max-score score))))))
    max-score))
