(defpackage :advent-of-code-2022/day-22
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:ppcre)
  (:import-from #:serapeum
                #:do-hash-table
                #:lines)
  (:import-from #:alexandria
                #:curry
                #:if-let)
  (:import-from #:trivia
                #:ematch))

(in-package :advent-of-code-2022/day-22)

(defconstant +cube-facet-neighbors+ 4)

(defstruct (board (:constructor board (rows cols &aux (data (make-array (list rows cols) :initial-element #\Space)))))
  (data nil :type simple-array))

(defstruct (state (:constructor state (board &optional direction (position (starting-position board))))
                  (:copier nil))
  (board nil :type board)
  (direction (list 0 1) :type list)
  (position nil :type list))

(defstruct (cubic-state (:include state)
                        (:copier nil))
  (edge-length 1 :type integer)
  (facet-corner-table nil :type hash-table)
  (corner-facet-table nil :type hash-table))

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

(defmethod copy-state ((state state))
  (state (state-board state)
         (copy-list (state-direction state))
         (copy-list (state-position state))))

(defmethod copy-state ((state cubic-state))
  (make-cubic-state
   :board (state-board state)
   :direction (copy-list (state-direction state))
   :position (copy-list (state-position state))
   :facet-corner-table (cubic-state-facet-corner-table state)
   :corner-facet-table (cubic-state-corner-facet-table state)
   :edge-length (cubic-state-edge-length state)))

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

(defmethod advance-state ((state state))
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

(defun direction-char (direction)
  (ematch direction
    ('(-1 0) #\^)
    ('(1 0) #\v)
    ('(0 -1) #\<)
    ('(0 1) #\>)))

(defun apply-move (state move)
  (let ((next-state (copy-state state)))
    (etypecase move
      (number (dotimes (i move)
                (setf (apply #'bref (state-board next-state) (state-position next-state))
                      (direction-char (state-direction next-state)))
                (setf next-state (advance-state next-state))))
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
    (setf (apply #'bref (state-board next-state) (state-position next-state))
          (direction-char (state-direction next-state)))
    next-state))

(defmethod print-object ((obj board) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (write-char #\Newline stream)
    (dotimes (row (board-rows obj))
      (dotimes (col (board-cols obj))
        (write-char (bref obj row col) stream))
      (write-char #\Newline stream))))

(defun final-password (state moves)
  (dolist (move moves)
    (setf state (apply-move state move)))
  (print (state-board state))
  (+ (* 1000 (1+ (state-row state)))
     (* 4 (1+ (state-col state)))
     (state-facing state)))

(defun part-1 (input)
  (destructuring-bind (board moves) (parse-input input)
    (let ((state (state board)))
      (final-password state moves))))

(defun part-2 (input &optional (edge-length 50))
  (destructuring-bind (board moves) (parse-input input)
    (let* ((corner-facet-table (fold-cube board edge-length))
           (state (make-cubic-state
                   :board board
                   :position (starting-position board)
                   :facet-corner-table (hash-table-inverse corner-facet-table)
                   :corner-facet-table corner-facet-table
                   :edge-length edge-length)))
      (final-password state moves))))

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
                        (list col (- row)))))
    (rotate-clockwise rotated-once (1- n))))

(defun fold-cube (board cube-edge-length)
  (do* ((corner-0 (starting-position board))
        (direction-0 (list (- cube-edge-length) 0))
        (corner-facet-table (make-hash-table :test 'equal))
        (stack (list (list :-z :-y corner-0 direction-0))))
       ((not stack) corner-facet-table)
    (destructuring-bind (facet previous-facet position direction) (pop stack)
      (setf (gethash position corner-facet-table) facet)
      (let* ((neighbors (cube-neighbors facet))
             (offset (+ 2 (position previous-facet neighbors))))
        (dotimes (i +cube-facet-neighbors+)
          (let ((next-facet (nth (mod (+ i offset) +cube-facet-neighbors+) neighbors))
                (next-position (list+ position direction)))
            (unless (or (apply #'out-of-bounds-p board next-position)
                        (gethash next-position corner-facet-table))
              (push (list next-facet facet next-position direction) stack))
            (setf direction (rotate-clockwise direction 1))))))))

(defun hash-table-inverse (table &key (test 'equal))
  (let ((inverse (make-hash-table :test test)))
    (do-hash-table (key value table inverse)
      (setf (gethash value inverse) key))))

(defun manhattan-distance (first second)
  (loop for x in first
        for y in second
        summing (abs (- x y))))

(defun nearest-facet (position facet-corner-table edge-length)
  (let ((best-corner nil)
        (best-facet nil)
        (best-distance edge-length))
    (do-hash-table (facet corner facet-corner-table)
      (let ((distance (manhattan-distance (mapcar (curry #'+ (/ edge-length 2)) corner)
                                          position)))
        (when (<= distance best-distance)
          (setf best-corner corner
                best-facet facet
                best-distance distance))))
    (values best-corner best-facet)))

(defmethod advance-state ((state cubic-state))
  (let* ((board (state-board state))
         (direction (state-direction state))
         (position (state-position state))
         (facet-corner-table (cubic-state-facet-corner-table state))
         (corner-facet-table (cubic-state-corner-facet-table state))
         (next-position (list+ direction (state-position state)))
         (next-direction direction)
         (edge-length (cubic-state-edge-length state)))
    (when (apply #'out-of-bounds-p board next-position)
      (multiple-value-bind (corner facet)
          (nearest-facet position facet-corner-table edge-length)
        (assert corner)
        (let ((neighbor-facet (find-neighbor-facet direction corner facet facet-corner-table edge-length)))
          (setf next-direction (mapcar (curry #'* -1) (find-edge-direction neighbor-facet facet facet-corner-table edge-length)))
          (let* ((rotation-offset (rotation-offset direction next-direction))
                 (corner->next-position (list- next-position corner))
                 (rotated-corner->next-position (rotate-clockwise corner->next-position rotation-offset))
                 (neighbor-corner (gethash neighbor-facet facet-corner-table))
                 (rotated-corner (list+ (list- neighbor-corner
                                               (mapcar (curry #'* edge-length) next-direction))
                                        (ecase rotation-offset
                                          (0 (list 0 0))
                                          (1 (list 0 (1- edge-length)))
                                          (2 (list (1- edge-length) (1- edge-length)))
                                          (3 (list (1- edge-length) 0))))))

            (setf next-position (list+ rotated-corner
                                       rotated-corner->next-position))))))
    (assert (not (apply #'out-of-bounds-p board next-position)))
    (if (char/= #\# (apply #'bref board next-position))
        (make-cubic-state
         :board board
         :direction next-direction
         :position next-position
         :edge-length edge-length
         :facet-corner-table facet-corner-table
         :corner-facet-table corner-facet-table)
        state)))

(defun find-edge-direction (source-facet target-facet facet-corner-table edge-length)
  (let ((source-center (gethash source-facet facet-corner-table)))
    (multiple-value-bind (anchor-facet anchor-center anchor-index)
        (find-anchor-facet source-center source-facet facet-corner-table edge-length)
      (declare (ignore anchor-facet))
      (assert anchor-index)
      (let* ((neighbors (cube-neighbors source-facet))
             (anchor-direction (mapcar #'signum (list- anchor-center source-center)))
             (target-index (position target-facet neighbors)))
        (assert target-index)
        (rotate-clockwise anchor-direction (mod (- target-index anchor-index) +cube-facet-neighbors+))))))

(defun rotation-offset (source target)
  (loop for i from 0 upto +cube-facet-neighbors+
        for d = source then (rotate-clockwise d 1)
        when (equal d target) do
          (return i)))

(defun find-anchor-facet (corner facet facet-corner-table edge-length)
  "Returns a neighbor facet that is also adjacent in the folded cube"
  (loop with neighbors = (cube-neighbors facet)
        for anchor-facet in neighbors
        for anchor-corner = (gethash anchor-facet facet-corner-table)
        for anchor-index = 0 then (1+ anchor-index)
        when (= edge-length (manhattan-distance anchor-corner corner)) do
          (return (values anchor-facet anchor-corner anchor-index))))

(defun find-neighbor-facet (direction corner facet facet-corner-table edge-length)
  (multiple-value-bind (anchor-facet anchor-corner anchor-index)
      (find-anchor-facet corner facet facet-corner-table edge-length)
    (declare (ignore anchor-facet))
    (let* ((anchor-direction (mapcar #'signum (list- anchor-corner corner)))
           (offset (rotation-offset anchor-direction direction))
           (neighbor-index (mod (+ offset anchor-index) +cube-facet-neighbors+))
           (neighbor-facet (nth neighbor-index (cube-neighbors facet))))
      neighbor-facet)))

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

10R5L5R10L4R5L5
")

(defparameter *board* (car (parse-input *example*)))
