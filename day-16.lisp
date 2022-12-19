(defpackage :advent-of-code-2022/day-16
  (:use :cl)
  (:import-from #:serapeum
                #:->
                #:lines
                #:words)
  (:import-from #:alexandria
                #:with-gensyms
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

(defmacro maximizing (&body body)
  (with-gensyms (called best best-rest value rest)
    `(let ((,called)
           (,best 0)
           (,best-rest))
       (labels ((maximize (,value &rest ,rest)
                  (when (or (not ,called)
                            (< ,best ,value))
                    (setf ,called t
                          ,best ,value
                          ,best-rest ,rest))))
         ,@body
         (values-list (cons ,best ,best-rest))))))

(defun part-1 (input)
  (destructuring-bind (*neighbors* *flows* id-table) (parse-input input)
    (let ((*dist* (floyd-warshall *neighbors*))
          (*cache* (make-hash-table :test 'equal))
          (*round* 0)
          (id (gethash "AA" id-table)))
      (helper id 30 0 0))))

(defparameter *cache* nil)
(defparameter *neighbors* nil)
(defparameter *flows* nil)
(defparameter *dist* nil)
(defparameter *initial-minutes* nil)
(defparameter *initial-node* nil)
(defparameter *round* nil)

(declaim ((or null fixnum) *round*)
         ((or null simple-array) *flows* *dist*))

(-> helper (fixnum fixnum fixnum fixnum) fixnum)
(defun helper (node minutes overlay round)
  (declare (fixnum node minutes overlay round)
           ((simple-vector) *flows*)
           ((simple-array t (* *)) *dist*))
  (when (<= minutes 0)
    (when (= round 0)
      (return-from helper 0))
    (return-from helper (helper *initial-node* *initial-minutes* overlay (1- round))))
  (let* ((next-overlay (logior overlay (ash 1 node)))
         (flow (if (= next-overlay overlay) 0 (aref *flows* node)))
         (key (list node minutes overlay round)))
    (declare (fixnum flow))
    (multiple-value-bind (cached present) (gethash key *cache*)
      (when present
        (return-from helper cached))
      (setf (gethash key *cache*)
            (maximizing
              (dotimes (next (length *flows*))
                (when (and (/= next node)
                           (not (logbitp next overlay))
                           (/= 0 (aref *flows* next)))
                  (let ((dist (aref *dist* node next)))
                    (declare (fixnum dist))
                    (when (and dist (>= minutes dist))
                      (maximize (helper next (- minutes dist) overlay round))
                      (when (and (>= minutes (1+ dist))
                                 (/= flow 0))
                        (maximize (+ (helper next
                                             (- minutes dist 1)
                                             next-overlay
                                             round)
                                     (* flow (1- minutes)))))))))
              (when (and (> minutes 0))
                (maximize (* flow (1- minutes)))))))))

(defun floyd-warshall (edges)
  "Constructs a distance matrix for the edge array under the assumption that every
edge has weight 1. If two vertices are not connected, their distance is set to `nil'"
  (declare (simple-vector edges))
  (let* ((n (length edges))
         (infinity (1+ n))
         (dist (make-array (list n n) :initial-element infinity)))
    (dotimes (i n)
      (dolist (j (aref edges i))
        (setf (aref dist i j) 1))
      (setf (aref dist i i) 0))
    (dotimes (k n)
      (dotimes (i n)
        (dotimes (j n)
          (let ((d (+ (aref dist i k)
                      (aref dist k j))))
            (when (> (aref dist i j) d)
              (setf (aref dist i j) d))))))
    (dotimes (i n)
      (dotimes (j n)
        (when (>= (aref dist i j) infinity)
          (setf (aref dist i j) nil))))
    dist))

(defun part-2 (input)
  (destructuring-bind (*neighbors* *flows* id-table) (parse-input input)
    (let ((*dist* (floyd-warshall *neighbors*))
          (*cache* (make-hash-table :test 'equal))
          (*initial-node* (gethash "AA" id-table))
          (*initial-minutes* 26))
      (helper *initial-node* *initial-minutes* 0 1))))

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
