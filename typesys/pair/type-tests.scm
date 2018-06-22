; Jeremy Batchelder and Morgan Ciliv
; Pair Programming Assignment: Typesystems
; Due: 3/16/18
; Document: type-tests.scm

; 1)
(check-type (lambda ([x : int]) (> x 2))  (int -> bool))

; 2)
(val x 8)
(check-type (begin x (let ([x 1] [y 2]) (+ x y))) int) 

; 3)
(check-type
  (letrec
    [([f1 : (int -> int)] (lambda ([x : int]) (+ x 1)))
     ([f2 : (int -> int)] (lambda ([x : int]) (+ x 2)))]
    f2)
  (int -> int))
