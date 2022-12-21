(defpackage :advent-of-code-2022/day-18
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:~>
                #:lines)
  (:import-from #:ppcre)
  (:import-from #:alexandria
                #:hash-table-keys
                #:with-gensyms
                #:curry))

(in-package :advent-of-code-2022/day-18)

(defun load-input ()
  (read-file-string "day-18.input"))

(defun parse-line (line)
  (~> (ppcre:split "," line)
      (mapcar #'parse-integer _)))

(defun parse-input (input)
  (~> (lines input)
      (mapcar #'parse-line _)))

(defun group-by (fn list &key (test 'equal))
  (let ((table (make-hash-table :test test)))
    (dolist (value list table)
      (let ((key (funcall fn value)))
        (push value (gethash key table nil))))))

(defun except-nth (pos list)
  (loop for i = 0 then (1+ i)
        for x in list
        when (/= i pos)
          collecting x))

(defun make-cube-graph (cubes)
  (let* ((graph (make-hash-table :test 'equal))
         (by-x (group-by #'car cubes))
         (by-y (group-by #'cadr cubes))
         (by-z (group-by #'caddr cubes))
         (by-dim (list by-x by-y by-z))
         (except-by-dim (mapcar (lambda (dim)
                                  (group-by (curry #'except-nth dim) cubes))
                                (list 0 1 2))))
    (dolist (cube cubes graph)
      (setf (gethash cube graph)
            (loop for dim-idx = 0 then (1+ dim-idx)
                  for dim-value in cube
                  for by-this-dim in by-dim
                  for except-by-this-dim in except-by-dim
                  appending (intersection (concatenate 'list
                                                      (gethash (1- dim-value) by-this-dim nil)
                                                      (gethash (1+ dim-value) by-this-dim nil))
                                          (gethash (except-nth dim-idx cube) except-by-this-dim)))))))

(defun cube-graph-surface-area (graph)
  (loop :for cube :being :the :hash-keys :of graph
        :using (:hash-value neighbors)
        :summing (- 6 (length neighbors))))

(defun part-1 (input)
  (~> (parse-input input)
      (make-cube-graph _)
      (cube-graph-surface-area _)))

(defun bounding-box (cubes)
  (let ((mins (loop for dim from 0 to 2 collecting (1- (loop for cube in cubes minimizing (nth dim cube)))))
        (maxs (loop for dim from 0 to 2 collecting (1+ (loop for cube in cubes maximizing (nth dim cube))))))
    (list mins maxs)))

(defun in-bounding-box-p (coordinate mins maxs)
  (loop for x in coordinate
        for min in mins
        for max in maxs
        always (<= min x max)))

(defun map-box-neighbors (fn cube)
  (dolist (delta '((+1 0 0)
                   (-1 0 0)
                   (0 +1 0)
                   (0 -1 0)
                   (0 0 +1)
                   (0 0 -1)))
    (funcall fn (mapcar #'+ cube delta))))

(defmacro do-cube-neighbors ((neighbor cube) &body body)
  `(map-box-neighbors (lambda (,neighbor) ,@body) ,cube))

(defun cube-graph-exterior-surface-area (graph)
  (let ((count 0)
        (cubes (hash-table-keys graph)))
    (destructuring-bind (mins maxs) (bounding-box cubes)
      (loop for cube in cubes do
        (do-cube-neighbors (neighbor cube)
          (if (and (not (gethash neighbor graph))
                   (can-escape-p neighbor graph mins maxs))
            (incf count)))))
    count))

(defun can-escape-p (cube graph mins maxs &optional (visited (make-hash-table :test 'equal)))
  (when (not (in-bounding-box-p cube mins maxs))
    (return-from can-escape-p t))
  (setf (gethash cube visited) t)
  (do-cube-neighbors (neighbor cube)
    (when (and (not (nth-value 1 (gethash neighbor graph)))
               (not (gethash neighbor visited))
               (can-escape-p neighbor graph mins maxs visited))
      (return-from can-escape-p t)))
  nil)

(defun part-2 (input)
  (~> (parse-input input)
      (make-cube-graph _)
      (cube-graph-exterior-surface-area _)))

(defparameter *example* "1,1,1
2,1,1")

(defparameter *large-example* "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")
