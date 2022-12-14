(defpackage :advent-of-code-2022/day-14
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:lines
                #:~>)
  (:import-from #:alexandria
                #:compose
                #:curry))

(in-package :advent-of-code-2022/day-14)

(defun parse-path (line)
  (mapcar (compose #'reverse
                   (curry #'mapcar #'parse-integer)
                   (curry #'ppcre:split ","))
          (ppcre:split " -> " line)))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-path _)))

(defun make-rock-table (rock-paths)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (path rock-paths table)
      (do ((path-iter path (cdr path-iter)))
          ((not (cadr path-iter)))
        (let* ((begin (car path-iter))
               (end (cadr path-iter))
               (step (mapcar (compose #'signum #'-) end begin)))
          (do ((curr begin (mapcar #'+ curr step)))
              (nil)
            (setf (gethash curr table) #\#)
            (when (equal curr end)
              (return))))))))

(defun rock-bottom (rock-table)
  (loop for pos being the hash-key of rock-table
          using (hash-value value)
        when (char= #\# value)
          maximizing (car pos)))

(defun drop-sand (rock-table position rock-bottom &optional prev)
  "Drop a grain of sand into `rock-table' starting at `position'. Returns `t' if
the grain found a stable position, otherwise `nil'."
  (when (>= (car position) rock-bottom)
    (return-from drop-sand (values nil position prev)))
  ;; try straight down if possible
  (let ((down (mapcar #'+ position '(+1 0))))
    (unless (gethash down rock-table)
      (return-from drop-sand (drop-sand rock-table down rock-bottom position))))
  ;; first fallback: diagonal left
  (let ((left (mapcar #'+ position '(+1 -1))))
    (unless (gethash left rock-table)
      (return-from drop-sand (drop-sand rock-table left rock-bottom position))))
  ;; second fallback: diagonal right
  (let ((right (mapcar #'+ position '(+1 +1))))
    (unless (gethash right rock-table)
      (return-from drop-sand (drop-sand rock-table right rock-bottom position))))
  ;; last fallback: stay in place
  (setf (gethash position rock-table) #\o)
  (values t position prev))

(defun part-1 (input)
 (let* ((rock-paths (parse-input input))
        (rock-table (make-rock-table rock-paths))
        (rock-bottom (rock-bottom rock-table)))
   (loop for count = 0 then (1+ count)
         while (drop-sand rock-table '(0 500) rock-bottom)
         finally (return count))))

(defun part-2 (input)
 (let* ((rock-paths (parse-input input))
        (rock-table (make-rock-table rock-paths))
        (rock-bottom (+ 2 (rock-bottom rock-table))))
   (loop for count = 1 then (1+ count)
         do (multiple-value-bind (rest-in-place position dropped-from) (drop-sand rock-table '(0 500) rock-bottom)
              (if rest-in-place
                  (when (equal '(0 500) position)
                    (return count))
                  (setf (gethash dropped-from rock-table) #\o))))))

(defun load-input ()
  (read-file-string "day-14.input"))

(defparameter *example*
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")
