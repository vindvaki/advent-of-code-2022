(defpackage :advent-of-code-2023/day-23
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
                #:flatten
                #:when-let
                #:define-constant
                #:curry
                #:if-let)
  (:import-from #:trivia
                #:ematch))

(in-package :advent-of-code-2023/day-23)

(defparameter *example* "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(defparameter *small-example* ".....
..##.
..#..
.....
..##.
.....")

(defun parse-input (input)
  (let ((elves (make-hash-table :test 'equal)))
    (loop for line in (lines input)
          for row = 0 then (1+ row) do
            (loop for c across line
                  for col = 0 then (1+ col)
                  when (char= c #\#) do
                    (setf (gethash (list row col) elves) t))
          finally (return elves))))

(define-constant +directions+
  '(((-1 0) (-1 +1) (-1 -1))  ; N NE NW
    ((+1 0) (+1 +1) (+1 -1))  ; S SE SW
    ((0 -1) (-1 -1) (+1 -1))  ; W NW SW
    ((0 +1) (-1 +1) (+1 +1))) ; E NE SE
  :test 'equal)

(defun neighborp (elves elf direction)
  (let ((neighbor (mapcar #'+ elf direction)))
     (gethash neighbor elves)))

(defun propose (elves elf directions)
  (unless (find-if (curry #'neighborp elves elf)
                   (apply #'concatenate 'list directions))
    (return-from propose))
  (dolist (direction-set directions)
    (let ((pivot (car direction-set)))
      (unless (find-if (curry #'neighborp elves elf)
                       direction-set)
        (return-from propose (mapcar #'+ pivot elf))))))

(defun proposals (elves directions)
  (let ((proposals (make-hash-table :test 'equal)))
    (do-hash-table (elf _ elves proposals)
      (declare (ignore _))
      (when-let ((proposal (propose elves elf directions)))
        (push elf (gethash proposal proposals))))))

(defun move-elves (elves proposals)
  (let ((result (make-hash-table :test 'equal))
        (moved (make-hash-table :test 'equal)))
    (do-hash-table (proposal proposers proposals)
      (when (= 1 (length proposers))
        (let ((elf (car proposers)))
          (setf (gethash proposal result) t
                (gethash elf moved) t))))
    (do-hash-table (elf _ elves)
      (declare (ignore _))
      (unless (gethash elf moved)
        (setf (gethash elf result) t)))
    (values result (hash-table-count moved))))

(defun simulate-rounds (elves directions &optional (limit -1))
  (loop with count = 0 do
    (multiple-value-bind (next-elves move-count)
        (move-elves elves (proposals elves directions))
      (setf elves next-elves
            directions (rotate-list (copy-list directions)))
      (incf count)
      (when (or (= 0 move-count) (= count limit))
        (return-from simulate-rounds (values elves directions count))))))

(defun bounding-box-area (elves)
  (let ((min-row)
        (max-row)
        (min-col)
        (max-col))
    (do-hash-table (elf _ elves)
      (declare (ignore _))
      (destructuring-bind (row col) elf
        (when (or (not min-row) (< row min-row))
          (setf min-row row))
        (when (or (not max-row) (> row max-row))
          (setf max-row row))
        (when (or (not min-col) (< col min-col))
          (setf min-col col))
        (when (or (not max-col) (> col max-col))
          (setf max-col col))))
    (* (1+ (- max-row min-row))
       (1+ (- max-col min-col)))))

(defun part-1 (input)
  (let ((elves (simulate-rounds (parse-input input)
                                +directions+
                                10)))
    (- (bounding-box-area elves)
       (hash-table-count elves))))

(defun part-2 (input)
  (nth-value 2 (simulate-rounds (parse-input input)
                                +directions+)))

(defun load-input ()
  (read-file-string "day-23.input"))
