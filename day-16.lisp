(defpackage :advent-of-code-2022/day-16
  (:use :cl)
  (:import-from #:serapeum
                #:lines
                #:words)
  (:import-from #:alexandria
                #:curry
                #:ensure-gethash)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2022/day-16)

(defun parse-line (string)
  (ppcre:register-groups-bind (node
                               (#'parse-integer flow)
                               (#'words neighbours))
    ("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
     string)
    (list node flow neighbours)))

(defun parse-input (input)
  (let* ((lines (lines input))
         (count (length lines))
         (neighbors (make-array count :element-type 'list :initial-element nil))
         (flows (make-array count :element-type 'integer :initial-element 0))
         (id-table (make-hash-table :test 'equal)))
    (labels ((ensure-id (string) (ensure-gethash string id-table (hash-table-count id-table))))
      (dolist (line lines)
        (destructuring-bind (node-name flow neighbor-names) (parse-line line)
          (let ((id (ensure-id node-name))
                (neighbor-ids (mapcar #'ensure-id neighbor-names)))
            (setf (aref neighbors id) neighbor-ids
                  (aref flows id) flow)))))
    (list neighbors
          flows
          id-table)))

(defun part-1 (input)
  (destructuring-bind (neighbors flows id-table) (parse-input input)
    (let ((cache (make-hash-table :test 'equal)))
      (labels ((helper (node minutes overlay &aux (flow (if (logbitp node overlay) 0 (aref flows node))))
                 (declare (fixnum node minutes flow overlay))
                 (when (<= minutes 0)
                   (return-from helper 0))
                 (when (= minutes 1)
                   (return-from helper flow))
                 ()
                 (let ((key (list node minutes overlay)))
                   (multiple-value-bind (cached present) (gethash key cache)
                     (when present
                       (return-from helper cached))
                     (setf (gethash key cache)
                           (loop for neighbor in (aref neighbors node)
                                 maximizing (if (= flow 0)
                                                (helper neighbor (- minutes 1) overlay)
                                                (max
                                                   (helper neighbor (- minutes 1) overlay)
                                                   (+ (* flow (1- minutes))
                                                      (helper neighbor
                                                              (- minutes 2)
                                                              (logior overlay (ash 1 node))))))))))))

        (helper (gethash "AA" id-table) 30 0)))))




(defun load-input ()
  (read-file-string "day-16.input"))

(defparameter *example*
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")
