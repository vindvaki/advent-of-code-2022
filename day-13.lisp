(defpackage :advent-of-code-2022/day-13
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:collecting)
  (:export
   :*example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-13)

(defun parse-bracket-list (string)
  (with-input-from-string (stream string)
    (read-bracket-list stream)))

(defun read-bracket-list (stream)
  (read-char stream)
  (do ((char (peek-char nil stream) (peek-char nil stream))
       (result nil))
      ((char= char #\]) (prog1 (reverse result)
                          (read-char stream)))
    (push
     (cond
       ((digit-char-p char) (read-integer stream))
       ((char= #\[ char) (read-bracket-list stream)))
     result)
    (when (char= #\, (peek-char nil stream))
      (read-char stream))))

(defun read-bracket-lists (stream)
  (collecting
    (loop do
      (let ((lhs (parse-bracket-list (read-line stream)))
            (rhs (parse-bracket-list (read-line stream))))
        ;; maybe consume two more lines
        (collect (list lhs rhs))
        (unless (read-line stream nil nil)
          (return))))))

(defun parse-input (input)
  (with-input-from-string (stream input)
    (read-bracket-lists stream)))

(defun read-integer (stream)
  (do* ((char (peek-char nil stream) (peek-char nil stream))
        (result 0))
       ((not (digit-char-p char)) result)
    (setf result (+ (* 10 result)
                    (- (char-code char) (char-code #\0))))
    (read-char stream)))

(defun <=> (first second)
  (cond ((< first second) -1)
        ((= first second) 0)
        (t +1)))

(defun compare-pair (lhs rhs)
  (cond
    ((not lhs) (if rhs -1 0))
    ((not rhs) +1)
    ((and (integerp lhs) (integerp rhs)) (<=> lhs rhs))
    ((integerp lhs) (compare-pair (list lhs) rhs))
    ((integerp rhs) (compare-pair lhs (list rhs)))
    ((and (listp lhs) (listp rhs))
     (ecase (compare-pair (car lhs) (car rhs))
       (-1 -1)
       (+1 +1)
       (0 (compare-pair (cdr lhs) (cdr rhs)))))))

(defun pair<= (first second)
  (>= 0 (compare-pair first second)))

(defun part-1 (input)
  (let ((pairs (parse-input input)))
    (loop for i = 1 then (1+ i)
          for (lhs rhs) in pairs
          when (= -1 (compare-pair lhs rhs))
            summing i)))

(defun part-2 (input)
  (let* ((pairs (append (apply #'append (parse-input input))
                        (list '((2)) '((6)))))
         (sorted (sort pairs #'pair<=))
         (first (1+ (position '((2)) sorted :test #'equal)))
         (second (1+ (position '((6)) sorted :test #'equal))))
    (* first second)))

(defun load-input ()
  (read-file-string "day-13.input"))

(defparameter *example* "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")
