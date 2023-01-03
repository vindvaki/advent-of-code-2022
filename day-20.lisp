(defpackage :advent-of-code-2022/day-20
  (:use :cl)
  (:import-from #:serapeum
                #:lines
                #:~>)
  (:import-from #:alexandria
                #:curry
                #:copy-array)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2022/day-20)

(defun mix (numbers &optional (mixed (copy-array numbers)))
  (let ((index-table (make-hash-table :test 'equal)))
    (loop for i = 0 then (1+ i)
          for x across mixed do
          (setf (gethash x index-table) i))
    (loop with n = (length numbers)
          for cons across numbers
          for x0 = (car cons)
          for x = (rem x0 (1- n))
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

(defun part-2 (input &optional (decryption-key 811589153))
  (let* ((numbers (map 'vector
                        (lambda (cons) (cons (* (car cons) decryption-key)
                                             (cdr cons)))
                        (parse-input input)))
         (mixed (mix numbers))
         (length (length numbers)))
    (dotimes (i 9)
      (setf mixed (mix numbers mixed)))
    (let ((zero (position-if (lambda (cons) (= 0 (car cons))) mixed)))
      (reduce #'+ (print (list (car (aref mixed (mod (+ zero 1000) length)))
                               (car (aref mixed (mod (+ zero 2000) length)))
                               (car (aref mixed (mod (+ zero 3000) length)))))))))

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
