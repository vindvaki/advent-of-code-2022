(defpackage :advent-of-code-2022/day-25
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:ppcre)
  (:import-from #:serapeum
                #:~>
                #:do-hash-table
                #:collecting
                #:hash-table-function
                #:lines)
  (:import-from #:alexandria
                #:when-let
                #:define-constant
                #:curry
                #:if-let)
  (:import-from #:trivia
                #:ematch))

(in-package :advent-of-code-2022/day-25)

(defparameter *example* "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(defun parse-snafu-digit (char)
  (ecase char
    (#\2 2)
    (#\1 1)
    (#\0 0)
    (#\- -1)
    (#\= -2)))

(defun parse-snafu (string)
  (loop with n = (length string)
        for i from 0 to (1- n)
        for c = (aref string i)
        for d = (parse-snafu-digit c)
        for s = (* (expt 5 (- n i 1)) d)
        summing s))

(defun write-snafu (number &optional (stream *standard-output*))
  (write-string
   (reverse
    (with-output-to-string (string-stream)
      (loop do
        (multiple-value-bind (rest digit) (floor (+ number 2) 5)
          (ecase digit
            (0 (write-char #\= string-stream))
            (1 (write-char #\- string-stream))
            (2 (write-char #\0 string-stream))
            (3 (write-char #\1 string-stream))
            (4 (write-char #\2 string-stream)))
          (setf number rest))
        until (= number 0))))
   stream))

(defun part-1 (input)
  (write-snafu (reduce #'+ (mapcar #'parse-snafu (lines input))) nil))

(defun load-input ()
  (read-file-string "day-25.input"))
