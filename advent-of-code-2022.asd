(defsystem :advent-of-code-2022
  :class :package-inferred-system
  :name "Hordur's solutions to Advent of Code 2022"
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-2022/day-1")
  :in-order-to ((test-op (test-op :advent-of-code-2022-tests))))

(register-system-packages "cl-ppcre" '(:ppcre))
