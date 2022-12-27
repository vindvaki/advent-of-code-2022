(defpackage :advent-of-code-2022/day-19
  (:use :cl)
  (:import-from #:alexandria
                #:copy-array
                #:compose
                #:hash-table-alist
                #:copy-hash-table
                #:hash-table-keys
                #:curry)
  (:import-from #:serapeum
                #:take
                #:lines
                #:dict
                #:~>)
  (:import-from #:fset)
  (:import-from #:trivia
                #:ematch)
  (:import-from #:uiop
                #:read-file-string))

(in-package :advent-of-code-2022/day-19)

(defconstant +resource-count+ 4)
(defconstant +ore+ 0)
(defconstant +clay+ 1)
(defconstant +obsidian+ 2)
(defconstant +geode+ 3)

(defun counts (ore clay obsidian geode)
  (make-array +resource-count+
              :element-type 'fixnum
              :initial-contents (list ore clay obsidian geode)))

(defun part-1 (input)
  (let ((blueprints (parse-blueprints input)))
    (loop for blueprint in blueprints
          do (print (blueprint-id blueprint))
          summing (quality-level blueprint))))

(defun part-2 (input)
  (~> (parse-blueprints input)
      (take 3 _)
      (mapcar (lambda (blueprint) (maximize-geodes blueprint 32)) _)
      (reduce #'* _)))

(defstruct blueprint
  (id 0 :type integer)
  (costs (make-array '(4 4) :element-type 'fixnum) :type array)
  (limits (counts 0 0 0 0) :type array))

(defstruct (state (:copier nil))
  (robots (counts 1 0 0 0) :type simple-array)
  (resources (counts 0 0 0 0) :type simple-array))

(defun copy-state (state)
  (make-state :robots (copy-array (state-robots state))
              :resources (copy-array (state-resources state))))

(defun neighbors (blueprint state max-time)
  "Returns the list of conses `(next . time)' of states `next' can be reached from
`state' by producing a robot in time `time', or by staying course for `max-time'."
  (serapeum:collecting
    ;; stay course
    (let ((next-state (copy-state state)))
      (dotimes (robot-id +resource-count+)
        (incf (aref (state-resources next-state) robot-id)
              (* max-time (aref (state-robots next-state) robot-id))))
      (collect (cons next-state max-time)))
    ;; produce a robot
    (dotimes (new-robot-id +resource-count+)
      (block build-robot-when-possible
        (let ((time 1)
              (next-state (copy-state state)))
          (incf (aref (state-robots next-state) new-robot-id))
          (when (and (/= new-robot-id +geode+)
                     (> (aref (state-robots next-state) new-robot-id)
                        (aref (blueprint-limits blueprint) new-robot-id)))
            ;; building more robots of this kind would not give better results
            (return-from build-robot-when-possible))
          (dotimes (dependency-id +resource-count+)
            ;; identify the time required to produce
            (let* ((cost (aref (blueprint-costs blueprint) new-robot-id dependency-id))
                   (available (aref (state-resources state) dependency-id))
                   (production (aref (state-robots state) dependency-id)))
              (cond
                ((>= available cost))
                 ;; dependency-id is not the limiting factor; nothing to do
                ((>= production 1)
                 ;; dependency-id needs production
                 (setf time (max time (1+ (ceiling (- cost available) production)))))
                (t (return-from build-robot-when-possible)))))
          ;; produce and pay cost
          (dotimes (dependency-id +resource-count+)
            (incf (aref (state-resources next-state) dependency-id)
                  (- (* time (aref (state-robots state) dependency-id))
                     (aref (blueprint-costs blueprint) new-robot-id dependency-id))))
          (when (< time max-time)
            (collect (cons next-state time))))))))

(defmethod fset:compare ((s1 state) (s2 state))
  (fset:compare-slots s1 s2 #'state-resources #'state-robots))

(defun state= (s1 s2)
  (fset:equal? s1 s2))

(defun state-sxhash (state)
  (sxhash (cons (coerce (state-robots state) 'list)
                (coerce (state-resources state) 'list))))

(sb-ext:define-hash-table-test state= state-sxhash)

(defun resource-count (state type)
  (aref (state-resources state) type))

(defun geode-count (state)
  (resource-count state +geode+))

(defun maximize-geodes (blueprint &optional (max-time 24))
  (let* ((source (make-state))
         (visited (make-hash-table :test #'state=))
         (stack (list (cons source 0)))
         (best source))
    (loop while stack do
      (destructuring-bind (state . time) (pop stack)
        (setf (gethash state visited) t)
        (let* ((remaining (- max-time time))
               (geode-upper-bound (* remaining
                                     (+ (/ (1- remaining) 2)
                                        (aref (state-robots state) +geode+)))))
          (when (>= (+ (geode-count state) geode-upper-bound)
                    (geode-count best))
            (if (= time max-time)
                (when (> (geode-count state)
                         (geode-count best))
                  (setf best state))
                (loop for (next-state . transition-time) in (neighbors blueprint state (- max-time time)) do
                  (unless (gethash next-state visited)
                    (push (cons next-state (+ time transition-time)) stack))))))))
    (geode-count best)))

(defun quality-level (blueprint &optional (max-time 24))
  (* (blueprint-id blueprint)
     (maximize-geodes blueprint max-time)))

(defun load-input ()
  (read-file-string "day-19.input"))

(defun parse-blueprint (string)
  (let ((blueprint (make-blueprint)))
    (destructuring-bind (id ore clay obsidian geode) (ppcre:split "[.:]" string)
      (parse-blueprint-id blueprint id)
      (parse-robot-cost blueprint ore)
      (parse-robot-cost blueprint clay)
      (parse-robot-cost blueprint obsidian)
      (parse-robot-cost blueprint geode)
      (dotimes (dependency-id +resource-count+)
        (dotimes (robot-id +resource-count+)
            (setf (aref (blueprint-limits blueprint) dependency-id)
                  (max (aref (blueprint-limits blueprint) dependency-id)
                       (aref (blueprint-costs blueprint) robot-id dependency-id))))))
    blueprint))

(defun parse-blueprints (string)
  (mapcar #'parse-blueprint (lines string)))

(defun parse-blueprint-id (blueprint string)
  (ppcre:register-groups-bind ((#'parse-integer blueprint-id))
      ("Blueprint (\\d+):?" string)
    (setf (blueprint-id blueprint) blueprint-id)))

(defun parse-amount-kind (string)
  (destructuring-bind (amount-string kind) (ppcre:split " " string)
      (list (parse-integer amount-string) kind)))

(defun resource-id (string)
  (ematch string
    ("ore" +ore+)
    ("clay" +clay+)
    ("obsidian" +obsidian+)
    ("geode" +geode+)))

(defun parse-robot-cost (blueprint string)
  (ppcre:register-groups-bind (kind costs-string)
      ("Each (\\w+) robot costs (.*)\\.?" string)
    (loop with id = (resource-id kind)
          with costs = (blueprint-costs blueprint)
          for amount-kind-string in (ppcre:split " and " costs-string)
          for (amount dependency-kind) = (parse-amount-kind amount-kind-string)
          for dependency-id = (resource-id dependency-kind)
          do (setf (aref costs id dependency-id) amount))))

(defparameter *example* "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defparameter *blueprints* (parse-blueprints *example*))
(defparameter *blueprint* (car *blueprints*))
