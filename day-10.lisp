(defpackage :advent-of-code-2022/day-10
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:lines)
  (:import-from #:ppcre)
  (:import-from #:trivia
                #:ematch)
  (:export
   :*small-example*
   :*large-example*
   :load-input
   :part-1
   :part-2))

(in-package :advent-of-code-2022/day-10)

(defun load-input ()
  (read-file-string "day-10.input"))

(defun parse-line (line)
  (let ((instruction (ppcre:split " " line)))
    (ematch (car instruction)
      ("noop" instruction)
      ("addx" (list (car instruction)
                    (parse-integer (cadr instruction)))))))

(defun parse-input (input)
  (mapcar #'parse-line (lines input)))

(defun map-cycles (cycle-callback instructions)
  (loop with register-x = 1
        with cycle-count = 0
        for instruction in instructions do
          (ematch (car instruction)
            ("noop" (progn
                      (funcall cycle-callback cycle-count register-x)
                      (incf cycle-count 1)))
            ("addx" (progn
                      (funcall cycle-callback cycle-count register-x)
                      (incf cycle-count 1)
                      (funcall cycle-callback cycle-count register-x)
                      (incf register-x (cadr instruction))
                      (incf cycle-count 1))))))

(defmacro do-cycles ((index register-x instructions) &body body)
  `(map-cycles (lambda (,index ,register-x) ,@body)
               ,instructions))

(defun part-1 (input &aux (checkpoints (list 20 60 100 140 180 220)))
  (let* ((checkpoint (pop checkpoints))
         (result 0))
    (do-cycles (index register-x (parse-input input))
      (unless checkpoint
        (return-from part-1 result))
      (when (= (1+ index) checkpoint)
        (incf result (* checkpoint register-x))
        (setf checkpoint (pop checkpoints))))
    result))

(defun part-2 (input &key (blank #\.) (visible #\#))
  (let ((screen (make-array '(6 40) :element-type 'character :initial-element blank)))
    (do-cycles (index register-x (parse-input input))
      (multiple-value-bind (round col) (floor index 40)
        (let ((row (mod round 6))
              (sprite-middle (mod register-x 40)))
          (when (<= (abs (- sprite-middle col)) 1)
            (setf (aref screen row col) visible)))))
    (with-output-to-string (stream)
      (print-screen screen stream))))

(defun print-screen (screen &optional (stream *standard-output*))
  (destructuring-bind (rows cols) (array-dimensions screen)
    (dotimes (row rows)
      (dotimes (col cols)
        (write-char (aref screen row col) stream))
      (unless (= row (1- rows))
        (write-char #\Newline stream)))))

(defparameter *small-example*
  "noop
addx 3
addx -5")

(defparameter *large-example*
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")
