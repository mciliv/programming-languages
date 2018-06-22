;; Comp105 Continuations Assignment, Problem 44 of Chapter 2

;; The program uses Scheme semantics if the last value evaluates to 2.
;; The program uses new Scheme semantics if the last value evaluates to 1.

(val y 1)
(val f (lambda () y))
(val y 2)
(f)