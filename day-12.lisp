(defpackage :advent-of-code-2022/day-12
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:enq
                #:deq
                #:queue-empty-p
                #:lines
                #:queue)
  (:export
   :*example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-12)

(defun parse-input (input)
  (let* ((lines (lines input))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array `(,rows ,cols) :element-type 'character :initial-contents lines)))

(defun grid-position (value grid &optional (test #'eq))
  (destructuring-bind (rows cols) (array-dimensions grid)
    (dotimes (row rows)
      (dotimes (col cols)
        (when (funcall test (aref grid row col) value)
          (return-from grid-position (list row col)))))))

(defun up-down-left-right (coordinate)
  (mapcar (lambda (delta) (mapcar #'+ delta coordinate))
          '((-1 0) (+1 0) (0 -1) (0 +1))))

(defun elevation (char)
  (case char
    (#\S (char-code #\a))
    (#\E (char-code #\z))
    (otherwise (char-code char))))

(defun fewest-steps (grid source target)
  (do* ((queue (queue (cons source nil)))
        (visited (make-hash-table :test 'equal)))
       ((queue-empty-p queue))
    (destructuring-bind (current . path) (deq queue)
      (setf (gethash current visited) t)
      (let ((current-value (apply #'aref grid current)))
        (when (equal current target)
          (return-from fewest-steps path))
        (dolist (next (up-down-left-right current))
          (when (and (apply #'array-in-bounds-p grid next)
                     (not (gethash next visited)))
            (let ((next-value (apply #'aref grid next)))
              (when (and (or (<= (elevation next-value)
                                 (1+ (elevation current-value)))))
                (setf (gethash next visited) t)
                (enq (cons next (cons current path)) queue)))))))))

(defun part-1 (input)
  (let* ((grid (parse-input input))
         (source (grid-position #\S grid #'char=))
         (target (grid-position #\E grid #'char=)))
    (length (fewest-steps grid source target))))

(defun part-2 (input)
  (let* ((grid (parse-input input))
         (target (grid-position #\E grid #'char=))
         (min-position (grid-position #\S grid #'char=))
         (min-steps (length (fewest-steps grid min-position target))))
    (destructuring-bind (rows cols) (array-dimensions grid)
     (dotimes (row rows)
       (dotimes (col cols)
         (when (char= #\a (aref grid row col))
           (let ((steps (length (fewest-steps grid (list row col) target))))
             (when (< 0 steps min-steps)
               (setf min-steps steps
                     min-position (list row col))))))))
    (values min-steps min-position)))

(defun load-input ()
  (read-file-string "day-12.input"))

(defparameter *example*
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")
