(defpackage :advent-of-code-2022/day-11
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:take
                #:~>
                #:lines
                #:queue)
  (:import-from #:ppcre)
  (:import-from #:trivia
                #:ematch)
  (:import-from #:alexandria
                #:compose
                #:curry
                #:flatten
                #:hash-table-values)
  (:export
   :load-input
   :part-1))
  ;;  :part-2))

(in-package :advent-of-code-2022/day-11)

(defun load-input ()
  (read-file-string "day-11.input"))

(defstruct monkey
  (index 0 :type integer)
  (items (queue) :type queue)
  (operation #'identity :type function)
  (test nil :type list))

(defun read-monkey-index (stream)
  (let ((line (read-line stream)))
    (ppcre:register-groups-bind ((#'parse-integer monkey-index))
        ("Monkey (\\d+)" line :sharedp t)
      monkey-index)))

(defun parse-comma-separated-integers (string)
  (~> (ppcre:split ", " string)
      (mapcar #'parse-integer _)))

(defun read-starting-items (stream)
  (let ((line (read-line stream)))
    (ppcre:register-groups-bind ((#'parse-comma-separated-integers starting-items))
        ("Starting items: ?(\\d+(, \\d+)*)" line :sharedp t)
      (apply #'queue starting-items))))

(defun parse-integer-or-string (string)
  (or (parse-integer string :junk-allowed t)
      string))

(defun parse-op (string)
  (ematch string
    ("+" #'+)
    ("*" #'*)))

(defun read-operation (stream)
  (let ((line (read-line stream)))
    (ppcre:register-groups-bind ((#'parse-integer-or-string lhs)
                                 (#'parse-op op)
                                 (#'parse-integer-or-string rhs))
        ("Operation: new = (old|\\d+) (\\+|\\*) (old|\\d+)" line)
      (ematch (list lhs rhs)
        ((list "old" "old") (lambda (old) (funcall op old old)))
        ((list "old" _) (lambda (old) (funcall op old rhs)))
        ((list _ "old") (lambda (old) (funcall op lhs old)))
        ((list _ _) (lambda (old) (declare (ignore old)) (funcall op lhs rhs)))))))

(defun parse-divisor (line)
  (ppcre:register-groups-bind ((#'parse-integer divisor))
      ("Test: divisible by (\\d+)" line)
    divisor))

(defun parse-if-true (line)
  (ppcre:register-groups-bind ((#'parse-integer monkey-index))
      ("If true: throw to monkey (\\d+)" line)
    monkey-index))

(defun parse-if-false (line)
  (ppcre:register-groups-bind ((#'parse-integer monkey-index))
      ("If false: throw to monkey (\\d+)" line)
    monkey-index))

(defun read-test (stream)
  (let ((divisor (parse-divisor (read-line stream)))
        (monkey-if-true (parse-if-true (read-line stream)))
        (monkey-if-false (parse-if-false (read-line stream))))
    (list divisor
          monkey-if-true
          monkey-if-false)))

(defun read-monkey (stream)
  (make-monkey
   :index (read-monkey-index stream)
   :items (read-starting-items stream)
   :operation (read-operation stream)
   :test (read-test stream)))

(defun read-empty-line (stream)
  (let ((line (read-line stream nil)))
    (prog1 line
      (when (and line (string/= line ""))
        (error "Expected empty line")))))

(defun read-monkey-table (stream)
  (loop with table = (make-hash-table)
        for monkey = (read-monkey stream)
        for empty-line = (read-empty-line stream) do
        (progn
          (setf (gethash (monkey-index monkey) table)
                monkey))
        while empty-line
        finally (return table)))

(defun parse-monkey-table (string)
  (with-input-from-string (stream string)
    (read-monkey-table stream)))

(defun simulate-monkeys (table &optional (rounds 20) (divisor 3))
  (let ((traffic (make-hash-table))
        (count (hash-table-count table))
        (lcm (apply #'lcm (mapcar (compose #'car #'monkey-test)
                                  (hash-table-values table)))))
    (dotimes (round rounds traffic)
      (dotimes (index count)
        (let* ((monkey (gethash index table))
               (items (serapeum:qlist (monkey-items monkey)))
               (operation (monkey-operation monkey))
               (test (monkey-test monkey)))
          (destructuring-bind (test-divisor if-true if-false) test
            (setf (monkey-items monkey) (queue))
            (dolist (worry-level items)
              (setf worry-level (floor (funcall operation worry-level) divisor))
              (when (= divisor 1)
                (setf worry-level (mod worry-level lcm)))
              (let* ((next-index (if (= 0 (mod worry-level test-divisor))
                                     if-true
                                     if-false))
                     (next-monkey (gethash next-index table)))
                (serapeum:enq worry-level (monkey-items next-monkey))))
            (incf (gethash index traffic 0)
                  (length items))))))))

(defun calculate-monkey-business (table rounds divisor)
  (~> (simulate-monkeys table rounds divisor)
      (hash-table-values _)
      (sort _ #'>=)
      (take 2 _)
      (apply #'* _)))

(defun part-1 (input)
  (~> (parse-monkey-table input)
      (calculate-monkey-business _ 20 3)))

(defun part-2 (input)
  (~> (parse-monkey-table input)
      (calculate-monkey-business _ 10000 1)))

(defparameter *example*
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")
