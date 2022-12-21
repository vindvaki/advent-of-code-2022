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

(defun simulate-tower (jets rock-count &optional
                                         (tower (make-array (list (* 4 rock-count) 7) :element-type 'character :initial-element #\.))
                                         (top 0)
                                         (rock-offset 0)
                                         (jet-index 0))
    (dotimes (iteration rock-count)
      (let ((rock (nth (mod (+ rock-offset iteration) (length *rocks*)) *rocks*)))
        (multiple-value-bind (next-top next-jet-index) (simulate-rock tower rock jets top jet-index)
          (setf top next-top
                jet-index next-jet-index))))
    (values top tower))

(defun part-1 (input)
  (nth-value 0 (simulate-tower input 2022)))

(defun find-tower-bottom (tower bottom top)
  (loop for col from 0 to 6
        minimizing
        (loop for row from (1+ top) downto bottom
              until (or (= row bottom)
                        (char= #\# (aref tower (1- row) col)))
              finally (return row))))

(defun part-2 (input &optional (rock-count 1000000000000))
  (let* ((rec (find-tower-recurrence input))
         (cycle-start (getf rec :next-seen))
         (cycle-length (- (getf rec :next-seen)
                          (getf rec :first-seen)))
         (top (getf rec :next-top))
         (tower-growth (- top
                          (getf rec :first-top)))
         (tower (getf rec :tower))
         (jet-index (getf rec :jet-index)))
    (multiple-value-bind (cycle-count remaining-rocks) (floor (- rock-count cycle-start 1) cycle-length)
      (let ((final-top (simulate-tower input remaining-rocks tower top (1+ cycle-start) jet-index)))
        (+ final-top (* cycle-count tower-growth))))))

(defun find-tower-recurrence (jets &optional (rock-count 5000))
  (let* ((cols 7)
         (rows (* 4 rock-count))
         (tower (make-array (list rows cols) :element-type 'character :initial-element #\.))
         (top 0)
         (bottom 0)
         (jet-index 0)
         (seen (make-hash-table :test 'equal)))
    (declare (fixnum rock-count cols rows top bottom jet-index))
    (dotimes (iteration rock-count)
      (let* ((rock-index (mod iteration (length *rocks*)))
             (rock (nth rock-index *rocks*)))
        (multiple-value-bind (next-top next-jet-index) (simulate-rock tower rock jets top jet-index)
          (setf top next-top
                bottom (find-tower-bottom tower bottom top)
                jet-index next-jet-index)
          (let* ((key (list rock-index
                            jet-index
                            (with-output-to-string (stream)
                              (print-tower tower top bottom stream))))
                 (last-iteration (gethash key seen)))
            (when last-iteration
              (return-from find-tower-recurrence (list
                                                  :first-seen (car last-iteration)
                                                  :first-top (cadr last-iteration)
                                                  :next-seen iteration
                                                  :next-top top
                                                  :jet-index jet-index
                                                  :tower tower)))
            (setf (gethash key seen) (list iteration top))))))))


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

(defun print-tower (tower top &optional (bottom 0) (stream *standard-output*))
  (loop for row from top downto bottom do
    (loop for col from 0 upto 6 do
      (write-char (aref tower row col) stream)
      finally (write-char #\Newline stream))))

(defun load-input ()
  (trim-whitespace (read-file-string "day-17.input")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-rock-types (string)
    (~> (ppcre:split "\\n\\n" string)
        (mapcar #'grid-from-string _))))

(defparameter *rocks* (parse-rock-types *rock-types-string*))
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
; 1559887005677
; 1514285714289
