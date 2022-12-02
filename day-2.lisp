(defpackage :advent-of-code-2022/day-2
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:words
                #:~>
                #:lines)
  (:export
   :load-input
   :part-1
   :part-2
   :*example*))

(in-package :advent-of-code-2022/day-2)

(defparameter *example* "A Y
B X
C Z")

(defun load-input ()
  (read-file-string "day-2.input"))

(defun parse-line (line)
  (destructuring-bind (a x) (words line)
    (list (position a '("A" "B" "C") :test 'equal)
          (position x '("X" "Y" "Z") :test 'equal))))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-line _)))

(defun winner (self)
  (mod (1+ self) 3))

(defun loser (self)
  (mod (1- self) 3))

(defun simple-score (round)
  (destructuring-bind (elf me) round
    (if (= elf me)
        (+ 3 (1+ me))
        (if (= (winner elf) me)
            (+ 6 (1+ me))
            (1+ me)))))

(defun part-1 (input)
  (~> (parse-input input)
      (mapcar #'simple-score _)
      (reduce #'+ _)))

(defun strategy-score (round)
  (destructuring-bind (elf outcome) round
    (ecase outcome
      (0 (1+ (loser elf)))
      (1 (+ 3 (1+ elf)))
      (2 (+ 6 (1+ (winner elf)))))))

(defun part-2 (input)
  (~> (parse-input input)
      (mapcar #'strategy-score _)
      (reduce #'+ _)))
