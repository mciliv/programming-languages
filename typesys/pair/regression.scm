; Jeremy Batchelder and Morgan Ciliv
; Pair Programming Assignment: Typesystems
; Due: 3/16/18
; Document: type-tests.scm

; step 5

(check-type 3 int)
(check-type-error #t3)

(check-type #t bool)
(check-type-error "1t"3)

(check-type 'hello sym)
(check-type-error 12a)

; step 6
(check-type (if #f 2 3) int)
(check-type-error (if 2 0 1))
(check-type-error (if #t #f 1))

; ;; step 9: Elabdef
(val y 1)
(check-type y int)
(check-type-error (val y #t3))

; step 9
(val b 10)
(check-type (set b 8) int)
(check-type-error (set x #t))

; step 10
(check-type (+ 2 2) int)
(check-type-error (+ 2 #f))
(check-type (> 2 2) bool)
(check-type-error (> 2 #t))

; step 11
(let ([x 1] [y 2]) (+ x y))
(check-type (let ([x 1] [y 2]) (+ x y)) int)

; step 12
(check-type (lambda ([x : int]) (> x 2))  (int -> bool))

; step 13
(val x 8)
(check-type (while (< x 10) (+ x 1)) unit)
(check-type (begin x (let ([x 1] [y 2]) (+ x y))) int) 
(val x 0)
(check-type (set x 8) int)
(check-type-error (set x #f))

; step 14
(check-type (let* ([x 1] [y 2]) (+ x y)) int)

; step 15
(check-type
  (letrec
    [([f1 : (int -> int)] (lambda ([x : int]) (+ x 1)))
     ([f2 : (int -> int)] (lambda ([x : int]) (+ x 2)))]
    f2)
  (int -> int))

; step 16
(val-rec  
  [f1 : (int -> int)]
  (lambda ([x : int]) 1))
(check-type f1 (int -> int))
(define int f2 ([x : int]) x)
(check-type f2 (int -> int))

; step 17
(check-type
  (type-lambda ['a] (lambda ([x : 'a]) x))
  (forall ('a) ('a -> 'a)))

; TYAPPLY tested in step 18

; step 18
((@ cons int) 4 (@ '() int))
(check-type
  ((@ cons int) 4 (@ '() int))
  (list int))

; step 19
(check-type (and #t #f) bool)





