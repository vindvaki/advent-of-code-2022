(defpackage :advent-of-code-2022-tests/tests
  (:use :cl :fiveam)
  (:local-nicknames (:day-1 :advent-of-code-2022/day-1))
  (:local-nicknames (:day-2 :advent-of-code-2022/day-2)))

(in-package :advent-of-code-2022-tests/tests)

(def-suite* tests)

(test regression-day-1
  (is (= 71924 (day-1:part-1 (day-1:load-input))))
  (is (= 210406 (day-1:part-2 (day-1:load-input)))))

(test unit-day-2
  (is (= 15 (day-2:part-1 day-2:*example*)))
  (is (= 12 (day-2:part-2 day-2:*example*))))

(test regression-day-2
  (is (= 11873 (day-2:part-1 (day-2:load-input))))
  (is (= 12014 (day-2:part-2 (day-2:load-input)))))
