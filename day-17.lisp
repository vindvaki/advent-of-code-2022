(defpackage :advent-of-code-2022/day-17
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:trim-whitespace
                #:->
                #:~>
                #:lines)
  (:import-from #:ppcre))

(in-package :advent-of-code-2022/day-17)

(defun grid-from-string (string)
  (let* ((lines (lines string))
         (rows (length lines))
         (cols (length (car lines))))
    (make-array (list rows cols) :initial-contents lines :element-type 'character)))

(defun simulate-tower (jets rock-count)
  (let* ((cols 7)
         (rows (* 4 rock-count))
         (rocks (parse-rock-types *rock-types-string*))
         (tower (make-array (list rows cols) :element-type 'character :initial-element #\.))
         (top 0)
         (jet-index 0))
    (dotimes (iteration rock-count)
      (let ((rock (nth (mod iteration (length rocks)) rocks)))
        (multiple-value-bind (next-top next-jet-index) (simulate-rock tower rock jets top jet-index)
          (setf top next-top
                jet-index next-jet-index))))
    (values top tower)))

(-> rock-intersect-p (simple-array simple-array fixnum fixnum) boolean)
(defun rock-intersect-p (tower rock row col)
  "Returns `t' if `rock' with bottom-edge at `row' and left-edge at `col' would
intersect the existing structure in `tower' (including floor and walls).
Otherwise, returns `nil'."
  (declare ((simple-array character (* *)) rock tower)
           (fixnum row col))
  (destructuring-bind (rock-rows rock-cols) (array-dimensions rock)
    (declare (fixnum rock-rows rock-cols))
    (dotimes (i rock-rows)
      (dotimes (j rock-cols)
        (when (or (not (array-in-bounds-p tower (+ row i) (+ col j)))
                  (and (char= #\# (aref rock (- rock-rows i 1) j))
                       (char= #\# (aref tower (+ row i) (+ col j)))))
          (return-from rock-intersect-p t))))))

(defun simulate-rock (tower rock jets top jet-index)
  (declare ((simple-array character (* *)) rock tower)
           (simple-string jets)
           (fixnum top))
  (let ((row (+ top 3))
        (col 2)
        (jet-count (length jets)))
    (declare (fixnum row col jet-count))
    ;; find stopping point
    (loop do
      (let ((next-col (+ col (ecase (aref jets jet-index)
                              (#\> +1)
                              (#\< -1)))))
        (unless (rock-intersect-p tower rock row next-col)
          (setf col next-col))
        (setf jet-index (mod (1+ jet-index) jet-count))
        (when (rock-intersect-p tower rock (1- row) col)
          (return))
        (decf row)))
    ;; write rock to tower
    (destructuring-bind (rock-rows rock-cols) (array-dimensions rock)
      (declare (fixnum rock-rows rock-cols))
      (dotimes (i rock-rows)
        (dotimes (j rock-cols)
          (let ((char (aref rock (- rock-rows i 1) j)))
            (when (char= char #\#)
              (setf (aref tower (+ row i) (+ col j))
                    char)))))
      (values (max top (+ row rock-rows))
              jet-index))))

(defun print-tower (tower top &optional (stream *standard-output*))
  (loop for row from top downto 0 do
    (loop for col from 0 upto 6 do
      (write-char (aref tower row col) stream)
      finally (write-char #\Newline))))

(defun parse-rock-types (string)
  (~> (ppcre:split "\\n\\n" string)
      (mapcar #'grid-from-string _)))

(defun load-input ()
  (trim-whitespace (read-file-string "day-17.input")))

(defparameter *rock-types-string* "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##")

(defparameter *example*
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
