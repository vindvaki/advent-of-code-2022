(defpackage :advent-of-code-2022-tests/tests
  (:use :cl :fiveam)
  (:local-nicknames (:day-1 :advent-of-code-2022/day-1))
  (:local-nicknames (:day-2 :advent-of-code-2022/day-2))
  (:local-nicknames (:day-3 :advent-of-code-2022/day-3))
  (:local-nicknames (:day-4 :advent-of-code-2022/day-4))
  (:local-nicknames (:day-5 :advent-of-code-2022/day-5))
  (:local-nicknames (:day-6 :advent-of-code-2022/day-6))
  (:local-nicknames (:day-7 :advent-of-code-2022/day-7))
  (:local-nicknames (:day-8 :advent-of-code-2022/day-8))
  (:local-nicknames (:day-9 :advent-of-code-2022/day-9))
  (:local-nicknames (:day-10 :advent-of-code-2022/day-10))
  (:local-nicknames (:day-11 :advent-of-code-2022/day-11))
  (:local-nicknames (:day-12 :advent-of-code-2022/day-12))
  (:local-nicknames (:day-13 :advent-of-code-2022/day-13)))

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

(test regression-day-6
  (is (= 1965 (day-6:part-1 (day-6:load-input))))
  (is (= 2773 (day-6:part-2 (day-6:load-input)))))

(test regression-day-7
  (is (= 1307902 (day-7:part-1 (day-7:load-input))))
  (is (= 7068748 (day-7:part-2 (day-7:load-input)))))

(test unit-day-8
  (is (= 21 (day-8:part-1 day-8:*example*)))
  (is (= 8 (day-8:part-2 day-8:*example*))))

(test regression-day-8
  (is (= 1845 (day-8:part-1 (day-8:load-input))))
  (is (= 230112 (day-8:part-2 (day-8:load-input)))))

(test unit-day-9
  (is (= 13 (day-9:part-1 day-9:*example*)))
  (is (= 1 (day-9:part-2 day-9:*example*)))
  (is (= 36 (day-9:part-2 day-9:*large-example*))))

(test regression-day-9
  (is (= 6367 (day-9:part-1 (day-9:load-input))))
  (is (= 2536 (day-9:part-2 (day-9:load-input)))))

(test unit-day-10
  (is (= 13140 (day-10:part-1 day-10:*large-example*)))
  (is (string= "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
         (day-10:part-2 day-10:*large-example*))))

(test regression-day-10
  (is (= 14520 (day-10:part-1 (day-10:load-input))))
  (is (string= "###..####.###...##..####.####...##.###..
#..#....#.#..#.#..#....#.#.......#.#..#.
#..#...#..###..#......#..###.....#.###..
###...#...#..#.#.##..#...#.......#.#..#.
#....#....#..#.#..#.#....#....#..#.#..#.
#....####.###...###.####.####..##..###.."
               (day-10:part-2 (day-10:load-input)))))

(test unit-day-11
  (is (= 10605 (day-11:part-1 day-11:*example*)))
  (is (= 2713310158 (day-11:part-2 day-11:*example*))))

(test regression-day-11
  (is (= 101436 (day-11:part-1 (day-11:load-input))))
  (is (= 19754471646 (day-11:part-2 (day-11:load-input)))))

(test unit-day-12
  (is (= 31 (day-12:part-1 day-12:*example*)))
  (is (= 29 (day-12:part-2 day-12:*example*))))

(test regression-day-12
  (is (= 490 (day-12:part-1 (day-12:load-input))))
  (is (= 488 (day-12:part-2 (day-12:load-input)))))

(test unit-day-13
  (is (= 13 (day-13:part-1 day-13:*example*)))
  (is (= 140 (day-13:part-2 day-13:*example*))))

(test regression-day-13
  (is (= 6272 (day-13:part-1 (day-13:load-input))))
  (is (= 22288 (day-13:part-2 (day-13:load-input)))))
