(defpackage :advent-of-code-2022/day-9
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:alexandria
                #:with-gensyms
                #:curry)
  (:import-from #:ppcre
                #:split)
  (:import-from #:trivia
                #:ematch)
  (:export
   :*example*
   :*large-example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-9)

(defparameter *example*
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defparameter *large-example*
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defun load-input ()
  (read-file-string "day-9.input"))

(defun parse-line (line)
  (destructuring-bind (direction amount) (split " " line)
    (list (parse-direction direction)
          (parse-integer amount))))

(defun parse-direction (direction)
  (ematch direction
    ("D" '(+1 0))
    ("U" '(-1 0))
    ("R" '(0 +1))
    ("L" '(0 -1))))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-line _)))

(defun next-position (position direction)
  (mapcar #'+ position direction))

(defun next-direction (head tail)
  (let* ((diff (mapcar #'- head tail))
         (should-move (find-if (lambda (d) (> (abs d) 1))
                               diff)))
    (mapcar (lambda (d) (if should-move
                            (signum d)
                            0))
            diff)))

(defun map-knot-steps (fn size steps)
  (let ((knots (loop for _ upto (1- size) collecting '(0 0))))
    (dolist (step steps)
      (destructuring-bind (initial-direction amount) step
        (dotimes (i amount)
          (do* ((old-head nil (car iter))
                (iter knots (cdr iter))
                (direction initial-direction (next-direction old-head (car iter))))
               ((not iter) ; until
                (funcall fn knots)) ; finally
            (setf (car iter) (next-position (car iter) direction))))))))

(defmacro do-knot-steps ((knots size steps) &body body)
  `(map-knot-steps (lambda (,knots) ,@body)
                   ,size
                   ,steps))

(defun count-visited (steps size)
  (let ((visited (make-hash-table :test 'equal)))
    (do-knot-steps (knots size steps)
      (setf (gethash (copy-list (last knots)) visited) t))
    (hash-table-count visited)))

(defun part-1 (input)
  (let ((steps (parse-input input)))
    (count-visited steps 2)))

(defun part-2 (input)
  (let ((steps (parse-input input)))
    (count-visited steps 10)))
