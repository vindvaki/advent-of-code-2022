(defsystem :advent-of-code-2022-tests
  :class :package-inferred-system
  :author "Hordur Freyr Yngvason"
  :license "MIT"
  :depends-on ("advent-of-code-2022-tests/tests")
  :perform (test-op (op c)
                    (symbol-call :advent-of-code-2022-tests/tests :run!
                                 (find-symbol* :tests :advent-of-code-2022-tests/tests))))
