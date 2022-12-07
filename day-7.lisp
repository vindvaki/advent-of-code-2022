(defpackage :advent-of-code-2022/day-7
  (:use :cl)
  (:import-from #:uiop
                #:read-file-string)
  (:import-from #:serapeum
                #:lines)
  (:import-from #:trivia
                #:ematch)
  (:export :load-input
           :part-1
           :part-2))

(in-package :advent-of-code-2022/day-7)

(defun load-input ()
  (read-file-string "day-7.input"))

(defparameter *example*
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defparameter *root* nil
  "The current filesystem root directory")

(defparameter *current* nil
  "The currently working directory")

(defstruct command
  (name "" :type string)
  (args nil :type list)
  (output nil :type list))

(defstruct node
  (name "" :type string)
  (parent nil :type (or null node)))

(defstruct (dir (:include node))
  (children (make-hash-table :test 'equal) :type hash-table))

(defstruct (file (:include node))
  (size 0 :type integer))

(defun parse-command (command-args)
  (ppcre:register-groups-bind (name args)
      ("\\$ (\\w+) ?(.*)" command-args)
    (make-command :name name
                  :args (ppcre:split " " args))))

(defun next-command (lines)
  (when lines
    (loop with command = (parse-command (car lines))
          for rest = (cdr lines) then (cdr rest)
          for line = (car rest)
          while (and line (char/= #\$ (aref line 0)))
          collecting line into output
          finally (progn
                    (setf (command-output command) output)
                    (return (list command rest))))))

(defun parse-input (input)
  (loop for (command rest) = (next-command (lines input)) then (next-command rest)
        while command
        collect command))

(defun reconstruct-tree (commands)
  (let* ((*root* (make-dir :name "/"))
         (*current* *root*))
    (dolist (command commands)
      (handle-command command))
    *root*))

(defun handle-command (command)
  (ematch (command-name command)
    ("ls" (handle-ls command))
    ("cd" (handle-cd command))))

(defun parse-node (ls-line)
  (destructuring-bind (dir-or-size name) (ppcre:split " " ls-line)
    (ematch dir-or-size
      ("dir" (make-dir :name name))
      (_ (make-file :name name :size (parse-integer dir-or-size))))))

(defun handle-ls (cmd)
  (dolist (line (command-output cmd))
    (let ((node (parse-node line))
          (current-children (dir-children *current*)))
      (setf (node-parent node) *current*
            (gethash (node-name node) current-children) node))))

(defun handle-cd (cmd)
  (ematch (command-args cmd)
    ((list path)
     (ematch path
       (".." (setf *current* (node-parent *current*)))
       ("/" (setf *current* *root*))
       (_ (setf *current* (gethash path (dir-children *current*))))))))

(defun calculate-size (node &optional callback)
  (let ((size (etypecase node
                (dir (loop for child being the hash-values of (dir-children node)
                           summing (calculate-size child callback)))
                (file (file-size node)))))
    (when callback
      (funcall callback node size))
    size))

(defun part-1 (input)
  (let* ((commands (parse-input input))
         (root (reconstruct-tree commands))
         (sum 0))
    (calculate-size
     root
     (lambda (node size)
       (when (and (dir-p node)
                  (<= size 100000))
         (incf sum size))))
    sum))

(defun part-2 (input &optional
                       (total-size 70000000)
                       (minimum-unused 30000000))
  (let* ((commands (parse-input input))
         (root (reconstruct-tree commands))
         (total-used (calculate-size root))
         (total-unused (- total-size total-used))
         (must-delete (max 0 (- minimum-unused total-unused)))
         (best-node root)
         (best-size total-used))
    (calculate-size
     root
     (lambda (node size)
       (when (and (dir-p node)
                  (>= best-size size must-delete))
         (setf best-size size
               best-node node))))
    (values best-size best-node)))
