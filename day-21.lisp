(defpackage :advent-of-code-2022/day-21
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:toposort
                #:lines)
  (:import-from #:alexandria
                #:hash-table-values
                #:ensure-gethash
                #:curry
                #:if-let)
  (:import-from #:trivia
                #:ematch))

(in-package :advent-of-code-2022/day-21)

(defun part-1 (input)
  (evaluate-node (parse-input input)))

(defun prepare-root-bisect (f x0)
  (loop
    with y0 = (funcall f x0)
    for x = (* 2 x0) then (* x 2)
    for y = (funcall f x)
    until (/= (signum y) (signum y0))
    finally (return x)))

(defun part-2 (input)
  (let ((graph (parse-input input)))
    (setf (car (gethash "root" graph)) "-")
    (let* ((f (compile nil (build-monkey-lambda graph "root" '("humn"))))
           (x0 2)
           (x1 (print (prepare-root-bisect f x0))))
      (round (root-bisect f x0 x1)))))

(defun root-bisect (f x0 x1)
  (when (< (abs (funcall f x0))
           (/ 1 100000))
    (return-from root-bisect x0))
  (let* ((y0 (funcall f x0))
         (x (/ (+ x0 x1) 2))
         (y (funcall f x)))
    (if (= (signum y) (signum y0))
        (root-bisect f x x1)
        (root-bisect f x0 x))))

(defun evaluate-node (graph &optional (node "root"))
  (let ((function (compile nil (build-monkey-lambda graph node))))
    (funcall function)))

(defun build-monkey-lambda (graph &optional (node "root") (variables nil))
  (let* ((symbols (make-hash-table :test 'equal))
         (expression (build-monkey-expression graph variables symbols node)))
    `(lambda ,(mapcar (lambda (v) (gethash v symbols)) variables)
       (declare (optimize (speed 3)))
       ,expression)))

(defun build-monkey-expression
    (graph &optional
             (variables nil)
             (symbols (make-hash-table :test 'equal))
             (node "root"))
  (if (find node variables :test #'string=)
      (ensure-gethash node symbols (gensym node))
      (let ((expr (gethash node graph)))
        (etypecase expr
          (number expr)
          (list (cons (ematch (car expr)
                        ("+" '+)
                        ("-" '-)
                        ("*" '*)
                        ("/" '/))
                      (mapcar (curry #'build-monkey-expression graph variables symbols)
                              (cdr expr))))))))

(defun parse-input (input)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (line (lines input) table)
      (destructuring-bind (source target) (ppcre:split ": " line)
        (setf (gethash source table)
          (handler-case (parse-integer target)
            (error ()
              (destructuring-bind (lhs op rhs) (ppcre:split " " target)
                (list op lhs rhs)))))))))

(defun load-input ()
  (read-file-string "day-21.input"))

(defparameter *example* "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")
