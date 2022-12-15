(defpackage :advent-of-code-2022/day-15
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:collecting
                #:lines
                #:~>)
  (:import-from #:ppcre)
  (:import-from #:alexandria
                #:curry
                #:compose))

(in-package :advent-of-code-2022/day-15)

(defun parse-sensor (string)
  (ppcre:register-groups-bind ((#'parse-integer sensor-x)
                               (#'parse-integer sensor-y)
                               (#'parse-integer beacon-x)
                               (#'parse-integer beacon-y))
      ("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
       string)
    `((,sensor-y ,sensor-x)
      (,beacon-y ,beacon-x))))

(defun manhattan-distance (first second)
  (reduce #'+ (mapcar (compose #'abs #'-) first second)))

(defun sensor-x-interval (sensor d y)
  "Let (y0 x0) = sensor. We want to find the intervals of x satisfying:

     |x - x0| + |y - y0| <= d
i.e. |x - x0| <= d - |y - y0|
i.e. |x - x0| <= c

This means that

* if c < 0, then there is no interval, because absolute values are >= 0
* if x >= x0, then x <= x0 + c
* if x <= x0, then x0 - x <= c
              i.e. x >= x0 - c

this means x0 - c <= x <= x0 + c when c >= 0"
  (declare (fixnum d y))
  (destructuring-bind (y0 x0) sensor
    (declare (fixnum y0 x0))
    (let ((c (- d (abs (- y y0)))))
      (declare (fixnum c))
      (when (>= c 0)
        (list (- x0 c) (+ x0 c))))))

(defun interval-union (list)
  "Returns a list of intervals representing the union of the intervals in `list'"
  (declare (type list list))
  (let ((begins (make-hash-table))
        (ends (make-hash-table))
        (events nil))
    (setf list (sort list #'<= :key #'car))
    (dolist (interval list)
      (destructuring-bind (begin end) interval
        (incf (the fixnum (gethash begin begins 0)))
        (incf (the fixnum (gethash end ends 0)))
        (push begin events)
        (push end events)))
    (setf events (remove-duplicates (sort events #'<=)))
    (collecting
      (let* ((begin (car events))
             (open-intervals (- (the fixnum (gethash begin begins 0))
                                (the fixnum (gethash begin ends 0)))))
        (declare (type fixnum begin open-intervals))
        (dolist (event (cdr events))
          (when (= open-intervals 0)
            (setf begin event))
          (incf open-intervals (- (the fixnum (gethash event begins 0))
                                  (the fixnum (gethash event ends 0))))
          (when (= open-intervals 0)
            (collect (list begin event))))))))

(defun all-sensor-x-intervals (sensor-beacon-list y)
  (interval-union
   (loop for (sensor beacon) in sensor-beacon-list
         for d = (manhattan-distance sensor beacon)
         for interval = (sensor-x-interval sensor d y)
         when interval
           collecting interval)))

(defun interval-length (interval)
  (abs (apply #'- interval)))

(defun part-1 (input &optional (y 2000000))
  (~> (lines input)
      (mapcar #'parse-sensor _)
      (all-sensor-x-intervals _ y)
      (mapcar #'interval-length _)
      (reduce #'+ _)))

(defun tuning-frequency (sensor)
  (destructuring-bind (y x) sensor
    (+ (* x 4000000) y)))

(defun part-2 (input &optional (max 4000000))
  (declare (type fixnum max))
  (let ((sensor-beacon-list (mapcar #'parse-sensor (lines input))))
    (dotimes (y max)
      (loop for ((a b) (c d)) on (all-sensor-x-intervals sensor-beacon-list y) do
        (when (and (<= (the fixnum b) max)
                   (>= (the fixnum c) 0))
          (let ((sensor (list y (1+ (the fixnum b)))))
            (return-from part-2 (values (tuning-frequency sensor) sensor))))))))

(defun load-input ()
  (read-file-string "day-15.input"))

(defparameter *example*
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")
