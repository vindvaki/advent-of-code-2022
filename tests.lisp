(defpackage :advent-of-code-2022-tests/tests
  (:use :cl :fiveam)
  (:local-nicknames (:day-1 :advent-of-code-2022/day-1))
  (:local-nicknames (:day-2 :advent-of-code-2022/day-2))
  (:local-nicknames (:day-3 :advent-of-code-2022/day-3))
  (:local-nicknames (:day-4 :advent-of-code-2022/day-4))
  (:local-nicknames (:day-5 :advent-of-code-2022/day-5)))

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

(test unit-day-3
  (is (= 157 (day-3:part-1 day-3:*example*)))
  (is (= 70 (day-3:part-2 day-3:*example*))))

(test regression-day-3
  (is (= 7553 (day-3:part-1 (day-3:load-input))))
  (is (= 2758 (day-3:part-2 (day-3:load-input)))))

(test unit-day-4
  (is (= 2 (day-4:part-1 day-4:*example*)))
  (is (= 4 (day-4:part-2 day-4:*example*))))

(test regression-day-4
  (is (= 471 (day-4:part-1 (day-4:load-input))))
  (is (= 888 (day-4:part-2 (day-4:load-input)))))

(test unit-day-5
  (is (string= "CMZ" (day-5:part-1 day-5:*example*)))
  (is (string= "MCD" (day-5:part-2 day-5:*example*))))

(test regression-day-5
  (is (string= "FWNSHLDNZ" (day-5:part-1 (day-5:load-input))))
  (is (string= "RNRGDNFQG" (day-5:part-2 (day-5:load-input)))))
