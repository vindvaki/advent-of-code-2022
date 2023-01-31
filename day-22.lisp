(defpackage :advent-of-code-2022/day-22
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:ppcre)
  (:import-from #:serapeum
                #:lines)
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:trivia
                #:ematch))

(in-package :advent-of-code-2022/day-22)

(defstruct (board (:constructor board (rows cols &aux (data (make-array (list rows cols) :initial-element #\Space)))))
  (data nil :type simple-array))

(defstruct (state (:constructor state (board &optional direction (position (starting-position board))))
                  (:copier nil))
  (board nil :type board)
  (direction (list 0 1) :type list)
  (position nil :type list))

(defun state-row (state)
  (car (state-position state)))

(defun state-col (state)
  (cadr (state-position state)))

(defun state-facing (state)
  (ematch (state-direction state)
    ((list 0 1) 0)
    ((list 1 0) 1)
    ((list 0 -1) 2)
    ((list -1 0) 3)))

(defun copy-state (state)
  (state (state-board state)
         (copy-list (state-direction state))
         (copy-list (state-position state))))

(defun starting-position (board)
  (dotimes (row (board-rows board))
    (dotimes (col (board-cols board))
      (when (char= #\. (bref board row col))
        (return-from starting-position (list row col))))))

(defun out-of-bounds-p (board row col)
  (or (not (array-in-bounds-p (board-data board) row col))
      (char= #\Space (bref board row col))))

(defun list+ (first &rest rest)
  (apply #'mapcar #'+ first rest))

(defun list- (first &rest rest)
  (apply #'mapcar #'- first rest))

(defun advance-state (state)
  (let* ((board (state-board state))
         (direction (state-direction state))
         (prev-position (state-position state))
         (next-position (list+ direction (state-position state))))
    (when (apply #'out-of-bounds-p board next-position)
      (loop do (setf next-position prev-position
                     prev-position (list- prev-position direction))
            until (apply #'out-of-bounds-p board prev-position)))
    (if (char/= #\# (apply #'bref board next-position))
        (state board
               direction
               next-position)
        state)))

(defun apply-move (state move)
  (let ((next-state (copy-state state)))
    (setf (apply #'bref (state-board state) (state-position next-state)) #\x)
    (etypecase move
      (number (dotimes (i move)
                (setf next-state (advance-state next-state))
                (setf (apply #'bref (state-board state) (state-position next-state)) #\x)))
      (string (setf (state-direction next-state)
                    (ematch move
                      ("L" (ematch (state-direction state)
                            ((list 0 1) (list -1 0))
                            ((list -1 0) (list 0 -1))
                            ((list 0 -1) (list 1 0))
                            ((list 1 0) (list 0 1))))
                      ("R" (ematch (state-direction state)
                             ((list 0 1) (list 1 0))
                             ((list 1 0) (list 0 -1))
                             ((list 0 -1) (list -1 0))
                             ((list -1 0) (list 0 1))))))))
    next-state))

(defmethod print-object ((obj board) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write-char #\Newline stream)
    (dotimes (row (board-rows obj))
      (dotimes (col (board-cols obj))
        (write-char (bref obj row col) stream))
      (write-char #\Newline stream))))

(defun final-password (board moves)
  (let ((state (state board)))
    (dolist (move moves)
      (setf state (apply-move state move)))
    (+ (* 1000 (1+ (state-row state)))
       (* 4 (1+ (state-col state)))
       (state-facing state))))

(defun board-rows (board)
  (array-dimension (board-data board) 0))

(defun board-cols (board)
  (array-dimension (board-data board) 1))

(defun bref (board row col)
  (aref (board-data board) row col))

(defun (setf bref) (new-value board row col)
  (setf (aref (board-data board) row col) new-value))

(defun parse-board (string)
  (let* ((board-lines (lines string))
         (board-rows (length board-lines))
         (board-cols (reduce #'max (mapcar #'length board-lines)))
         (board (board board-rows board-cols)))
    (loop for line in board-lines
          for row = 0 then (1+ row) do
            (loop for c across line
                  for col = 0 then (1+ col) do
                    (setf (bref board row col) c)))
    board))

(defun cube-neighbors (facet)
  (ecase facet
    (:-z '(:+y :-x :-y :+x))
    (:+x '(:+z :+y :-z :-y))
    (:-y '(:+z :+x :-z :-x))
    (:-x '(:+z :-y :-z :+y))
    (:+y '(:+z :-x :-z :+x))
    (:+z '(:-y :-x :+y :+x))))

(defun rotate-clockwise (list n)
  "Rotate 90 degrees clockwise around the origin"
  (when (= n 0)
    (return-from rotate-clockwise list))
  (let ((rotated-once (destructuring-bind (row col) list
                        (list (- col) row))))
    (rotate-clockwise rotated-once (1- n))))

(defun unfold-cube (board cube-edge-length)
  (do* ((corner-0 (starting-position board))
        (center-0 (list+ corner-0 (list (/ cube-edge-length 2)
                                        (/ cube-edge-length 2))))
        (direction-0 (list (- cube-edge-length) 0))
        (visited (make-hash-table :test 'equal))
        (stack (list (list :-z :-y center-0 direction-0))))
       ((not stack) visited)
    (destructuring-bind (facet previous-facet position direction) (pop stack)
      (setf (gethash position visited) facet)
      (let* ((neighbors (cube-neighbors facet))
             (offset (+ 2 (position previous-facet neighbors))))
        (dotimes (i 4)
          (let ((next-facet (nth (mod (+ i offset) 4) neighbors))
                (next-position (list+ position direction)))
            (print next-position)
            (unless (or (apply #'out-of-bounds-p board next-position)
                        (gethash next-position visited))
              (push (list next-facet facet next-position direction) stack))
            (setf direction (rotate-clockwise direction 1))))))))

(defun parse-moves (string)
  (loop with start = 0
        until (= start (1- (length string)))
        collect (multiple-value-bind (match-start match-end)
                    (ppcre:scan "[LR]|(\\d+)" string :start start)
                  (setf start match-end)
                  (let ((match (subseq string match-start match-end)))
                    (if-let ((number (parse-integer match :junk-allowed t)))
                      number
                      match)))))

(defun parse-input (input)
  (destructuring-bind (board-string move-string) (ppcre:split "\\n\\n" input)
    (list (parse-board board-string)
          (parse-moves move-string))))

(defun load-input ()
  (read-file-string "day-22.input"))

(defparameter *example* "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(defparameter *board* (car (parse-input *example*)))
