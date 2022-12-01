(defpackage :advent-of-code-2022-tests/tests
  (:use :cl :fiveam)
  (:local-nicknames (:day-1 :advent-of-code-2022/day-1)))

(in-package :advent-of-code-2022-tests/tests)

(def-suite* tests)

(test regression-day-1
  (is (= 71924 (day-1:part-1 (day-1:load-input))))
  (is (= 210406 (day-1:part-2 (day-1:load-input)))))
