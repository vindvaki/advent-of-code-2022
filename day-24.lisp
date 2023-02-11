(defpackage :advent-of-code-2022/day-24
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

(in-package :advent-of-code-2022/day-24)

(defstruct state
  positions
  source
  target
  rows
  cols
  blizzards)

(defun state-target-p (state position)
  (equal position (state-target state)))

(defun state-source-p (state position)
  (equal position (state-source state)))

(defun state-in-human-bounds-p (state position)
    (or (state-target-p state position)
        (state-source-p state position)
        (state-in-blizzard-bounds-p state position)))

(defun state-in-blizzard-bounds-p (state position)
  (destructuring-bind (row col) position
    (and (<= 0 row (1- (state-rows state)))
         (<= 0 col (1- (state-cols state))))))

(defun next-blizzard-position (state position direction)
  (destructuring-bind (row col) position
    (destructuring-bind (row-inc col-inc) direction
      (list
       (mod (+ row row-inc) (state-rows state))
       (mod (+ col col-inc) (state-cols state))))))

(defun next-blizzards (state)
  (let ((result (make-hash-table :test 'equal)))
    (do-hash-table (position directions (state-blizzards state) result)
      (dolist (direction directions)
        (let ((next-position (next-blizzard-position state position direction)))
          (push direction (gethash next-position result)))))))

(defun neighbors (position)
  (destructuring-bind (row col) position
    (list position
          (list (1+ row) col)
          (list (1- row) col)
          (list row (1+ col))
          (list row (1- col)))))

(defun next-state (state)
  (let* ((next-blizzards (next-blizzards state))
         (next-positions (collecting
                           (dolist (position (state-positions state))
                             (dolist (neighbor (neighbors position))
                               (when (and (state-in-human-bounds-p state neighbor)
                                          (not (gethash neighbor next-blizzards)))
                                 (collect neighbor)))))))
    (make-state
     :positions (remove-duplicates next-positions :test 'equal)
     :blizzards next-blizzards
     :source (state-source state)
     :target (state-target state)
     :rows (state-rows state)
     :cols (state-cols state))))

(defun shortest-path (state)
  (loop with target = (state-target state)
        for count = 0 then (1+ count)
        until (find target (state-positions state) :test 'equal) do
          (setf state (next-state state))
        finally (return (values count state))))

(defun parse-direction (char)
  (case char
    (#\> '(0 +1))
    (#\< '(0 -1))
    (#\^ '(-1 0))
    (#\v '(+1 0))))

(defun parse-input (input)
  (loop with lines = (lines input)
        with source = (list -1 (1- (position #\. (car lines))))
        with target = (list (- (length lines) 2) (1- (position #\. (car (last lines)))))
        with blizzards = (make-hash-table)
        with rows = (- (length lines) 2)
        with cols = (- (length (car lines)) 2)
        for row = 0 then (1+ row)
        for line in lines do
        (loop for col = 0 then (1+ col)
              for char across line
              for direction = (parse-direction char)
              when direction
                do (push direction (gethash (list (1- row) (1- col)) blizzards)))
        finally (return (make-state :positions (list source)
                                    :source source
                                    :target target
                                    :rows rows
                                    :cols cols
                                    :blizzards blizzards))))

(defun load-input ()
  (read-file-string "day-24.input"))

(defun part-1 (input)
  (shortest-path (parse-input input)))

(defun part-2 (input)
  (let* ((state-0 (parse-input input))
         (state state-0)
         (total-distance 0))
    (multiple-value-bind (distance next-state) (shortest-path state)
      (setf (state-positions next-state) (list (state-target state-0))
            (state-target next-state) (state-source state-0)
            (state-source next-state) (state-target state-0)
            state next-state)
      (incf total-distance distance))
    (multiple-value-bind (distance next-state) (shortest-path state)
      (setf (state-positions next-state) (list (state-source state-0))
            (state-source next-state) (state-source state-0)
            (state-target next-state) (state-target state-0)
            state next-state)
      (incf total-distance distance))
    (incf total-distance (shortest-path state))
    total-distance))

(defparameter *example* "#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#")

(defparameter *complex-example* "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")
