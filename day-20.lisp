(defpackage :advent-of-code-2022/day-20
  (:use :cl)
  (:import-from #:serapeum
                #:lines
                #:~>)
  (:import-from #:alexandria
                #:copy-array)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2022/day-20)

(defun mix (numbers)
  (let ((mixed (copy-array numbers))
        (index-table (make-hash-table :test 'equal)))
    (loop for i = 0 then (1+ i)
          for x across mixed do
          (setf (gethash x index-table) i))
    (loop with n = (length numbers)
          for cons across numbers
          for x = (car cons)
          for d = (signum x)
          for i = (gethash cons index-table) do
            (loop for y = 0 then (+ y d)
                  for k0 = (mod (+ i y) n)
                  for k1 = (mod (+ i y d) n)
                  until (= y x) do
                    (progn
                      (rotatef (aref mixed k0) (aref mixed k1))
                      (setf (gethash (aref mixed k0) index-table) k0
                            (gethash (aref mixed k1) index-table) k1))))
    mixed))

(defun part-1 (input)
  (let* ((numbers (parse-input input))
         (length (length numbers))
         (mixed (mix numbers))
         (zero (position-if (lambda (cons) (= 0 (car cons))) mixed)))
    (reduce #'+ (print (list (car (aref mixed (mod (+ zero 1000) length)))
                             (car (aref mixed (mod (+ zero 2000) length)))
                             (car (aref mixed (mod (+ zero 3000) length))))))))

(defun parse-input (input)
  (loop with lines = (lines input)
        with result = (make-array (length lines))
        for line in lines
        for number = (parse-integer line)
        for i = 0 then (1+ i) do
          (setf (aref result i) (cons number i))
        finally (return result)))

(defun load-input ()
  (read-file-string "day-20.input"))

(defparameter *example* "1
2
-3
3
-2
0
4")
