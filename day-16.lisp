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

(defun part-2 (input)
  (destructuring-bind (neighbors flows id-table) (parse-input input)
    (let ((cache (make-hash-table :test 'equal)))
      (labels ((helper (u v minutes overlay &aux
                                              (u-flow (if (logbitp u overlay) 0 (aref flows u)))
                                              (v-flow (if (logbitp v overlay) 0 (aref flows v))))
                 (declare (fixnum u v minutes overlay u-flow v-flow))
                 (when (<= minutes 0)
                   (return-from helper 0))
                 (when (= minutes 1)
                   (return-from helper (+ u-flow v-flow)))
                 (when (= (length flows) (logcount overlay))
                   (return-from helper 0))
                 (when (> v u)
                   (return-from helper (helper v u minutes overlay)))
                 (let ((key (list u v minutes overlay)))
                   (multiple-value-bind (cached present) (gethash key cache)
                     (when present
                       (return-from helper cached))
                     (setf (gethash key cache)
                           (max
                            ;; neither opens
                            (loop for un in (aref neighbors u)
                                  maximizing
                                  (loop for vn in (aref neighbors v)
                                        maximizing (helper un vn (- minutes 1) overlay)))
                            ;; only u opens
                            (if (= 0 u-flow)
                                0
                                (loop for vn in (aref neighbors v)
                                      maximizing
                                      (+ (* u-flow (1- minutes))
                                         (helper u vn
                                                 (- minutes 1)
                                                 (logior overlay (ash 1 u))))))
                            ;; only v opens
                            (if (= 0 v-flow)
                                0
                                (loop for un in (aref neighbors u)
                                      maximizing
                                      (+ (* v-flow (1- minutes))
                                         (helper un v
                                                 (- minutes 1)
                                                 (logior overlay (ash 1 v))))))
                            ;; both open
                            (if (or (= 0 u-flow) (= 0 v-flow) (= u v))
                                0
                                (+ (* u-flow (1- minutes))
                                   (* v-flow (1- minutes))
                                   (helper u v
                                           (1- minutes)
                                           (logior overlay (ash 1 u) (ash 1 v)))))))))))
        (helper (gethash "AA" id-table)
                (gethash "AA" id-table)
                26 0)))))

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
