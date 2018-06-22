; 3 tests will test only class Natural.

; Summary: plus:carry:, different degree, carry bit, growth
(check-expect
  (from:  Array (decimal (plus:carry: (new: Natural 254) (new: Natural 1) 1)))
  '(2 5 6))

; Summary: minus:borrow:, different degree, a carry bit
(check-expect
  (from: Array (decimal (minus:borrow: (new: Natural 256) (new: Natural 1) 1)))
  '(2 5 4))

;Summary: divmod:with:, negative divisor, resulting in error
(check-expect
  (from: Array (decimal (mod: (new: Natural 17) 256)))
  '(1 7))

; 3 tests will test the large-integer classes, which are built on top of class Natural.

; Summary: Multiply all combinations of signs
(check-expect
  (decimal
    (magnitude
      (*
        (new: LargeInteger 2)
        (*
          (new: LargeInteger -2)
          (*
            (new: LargeInteger 2)
            (*
              (new: LargeInteger -3)
              (new: LargeInteger -2)))))))
  '(4 8))

; Summary: .........
(check-expect
  (decimal
    (magnitude
      (+
        (new: LargeInteger -10)
        (new: LargeInteger 3))))
  '(7))

; Summary: .........
(check-expect
  (decimal
    (magnitude
      (*
        (+
          (new: LargeInteger 3)
          (new: LargeInteger 2))
        (new: LargeInteger 2))))
  '(1 0))

; 3 tests will test mixed arithmetic and comparison involving both small and large integers.

; Summary: 
(check-expect
  (decimal
    (magnitude
      (*
        (new: LargeInteger 9)
        (new: SmallInteger 2))))
  '(1 8))


; Summary: .........
(check-expect
  (decimal
    (magnitude
      (-
        (new: LargeInteger 4)
        (new: SmallInteger 2))))
  '(2))

; Summary: .........
(check-expect
  (decimal
    (=
      (new: LargeInteger 9)
      (new: SmallInteger 2)))
  false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect
  (decimal
    (=
      (new: LargeInteger 2)
      (new: SmallInteger 2)))
  true)

(check-expect
  (decimal
    (=
      (new: SmallInteger 2)
      (new: SmallInteger 2)))
  true)

(check-expect
  (decimal
    (=
      (new: SmallInteger 3)
      (new: SmallInteger 2)))
  false)

;; 2.

; Summary: 
(check-expect
  (decimal
    (magnitude
      (*
        (new: SmallInteger 9)
        (new: LargeInteger 2))))
  '(1 8))


(check-expect
  (decimal
    (magnitude
      (-
        (new: LargeInteger 4)
        (negated (new: SmallInteger 2)))))
  '(6))

(check-expect
  (decimal
    (magnitude
      (+
        (new: LargeInteger 4)
        (new: SmallInteger 2))))
  '(6))


(check-expect
  (decimal
    (magnitude
      (+
        (negated (new: LargeInteger 4))
        (negated (new: SmallInteger 2)))))
  '(6))

(check-expect
  (decimal
    (magnitude
      (*
        (* (new: LargeInteger -3)
           (new: LargeInteger -2))
        (new: LargeInteger 2))))
  '(1 2))

(check-expect
  (decimal
    (magnitude
      (*
        (* (new: LargeInteger -3)
           (new: LargeInteger 2))
        (new: LargeInteger 2))))
  '(1 2))

(check-expect
  (decimal
    (magnitude
      (*
        (* (new: LargeInteger 3)
           (new: LargeInteger 2))
        (new: LargeInteger 2))))
  '(1 2))



(check-expect
  (decimal
    (magnitude
      (>
        (new: LargeInteger 9)
        (new: SmallInteger 2))))
  true)


(check-expect
  (decimal
    (magnitude (new: LargeInteger 100)))
  '(1 0 0))
; (check-expect
;   (decimal
;     (* (new: Natural 100) (new: Natural 1000)))
;   '(1 0 0 0 0 0))

; (check-expect
;   (decimal
;     (*
;       (* (new: Natural 100)
;          (new: Natural 1000))
;       (new: Natural 1000)))
;   '(1 0 0 0 0 0 0 0 0))


(val a1 (new: Natural 256))
(val b1 (new Natural))
(val c1 (new: Natural 233))

(check-assert (not (isZero a1)))
(check-assert      (isZero b1))
(check-assert (not (isZero c1)))

(check-expect (toInt a1) 256)
(check-expect (toInt b1) 0)
(check-expect (toInt c1) 233)

(check-expect (toInt (divBase a1)) 16)
(check-expect (toInt (divBase b1)) 0)
(check-expect (toInt (divBase c1)) 14)

(check-expect (modBase a1) 0)
(check-expect (modBase b1) 0)
(check-expect (modBase c1) 14)

(check-expect (toInt (timesBase a1)) 256)
(check-expect (toInt (timesBase b1)) 0)
(check-expect (toInt (timesBase c1)) 224)

; bigger degree first, no carry bit
(val a (new: Natural 256))
(val b (new Natural))
; different degree, no carry bit, no growth
(check-expect (toInt (plus:carry: a b 0)) 256)

; bigger degree second, no carry bit
(val c (new Natural))
(val d (new: Natural 256))
; different degree, no carry bit, no growth
(check-expect (toInt (plus:carry: c d 0)) 256)

; bigger degree first, carry bit
(val e (new: Natural 256))
(val f (new Natural))
; different degree, carry bit, no growth
(check-expect (toInt (plus:carry: e f 1)) 257)
; (check-expect (toInt (minus:borrow: e f 1)) 254)

; bigger degree second, carry bit
; different degree, carry bit, no growth
(val g (new Natural))
(val h (new: Natural 256))
(check-expect (toInt (plus:carry: g h 1)) 257)

; same degree, no carry bit, growth
(val i (new: Natural 15))
(val j (new: Natural 1))
(check-expect (toInt (plus:carry: i j 0)) 16)

; same degree, carry bit, growth
(val k (new: Natural 14))
(val l (new: Natural 1))
(check-expect (toInt (plus:carry: k l 1)) 16)

; same degree, no carry bit, no growth
(val m (new: Natural 12))
(val n (new: Natural 1))
(check-expect (toInt (plus:carry: m n 0)) 13)

; same degree, carry bit, no growth
(val o (new: Natural 12))
(val p (new: Natural 1))
(check-expect (toInt (plus:carry: o p 1)) 14)

; different degree, no carry bit, growth
(val q (new: Natural 255))
(val r (new: Natural 15))
(check-expect (toInt (plus:carry: q r 0)) 270)

; different degree, carry bit, growth
(val s (new: Natural 255))
(val t (new: Natural 15))
(check-expect (toInt (plus:carry: s t 1)) 271)

; zero zero
(val u (new: Natural 0))
(val v (new: Natural 0))
(check-expect (toInt (plus:carry: u v 0)) 0)

; zero zero carry bit
(val w (new: Natural 0))
(val x (new: Natural 0))
(check-expect (toInt (plus:carry: w x 1)) 1)

; different degree, carry bit, growth
(val y (new: Natural 254))
(val z (new: Natural 1))
(check-expect (toInt (plus:carry: y z 1)) 256)

;; Minus

; Different degree, no carry bit
(val a2 (new: Natural 256))
(val b2 (new Natural))
(check-expect (toInt (minus:borrow: a2 b2 0)) 256)

; Same degree, no carry bit
(check-expect
  (toInt
    (minus:borrow: (new: Natural 256) (new: Natural 256) 0))
  0)

; Same degree, with a carry bit
(check-expect
  (toInt
    (minus:borrow: (new: Natural 256) (new: Natural 255) 1))
  0)

; Different degree, with a carry bit
(check-expect
  (toInt
    (minus:borrow: (new: Natural 256) (new: Natural 1) 1))
  254)

; Raise error same degree
(check-error
  (minus:borrow: (new: Natural 256) (new: Natural 257) 1))

; Raise error different degree
(check-error
  (minus:borrow: (new: Natural 16) (new: Natural 257) 1))

(val c2 (new: Natural 256))
(val d2 (new: Natural 111))
(check-expect (toInt (minus:borrow: c2 d2 0)) 145)

(val e2 (new: Natural 256))
(val f2 (new: Natural 111))
(check-expect (toInt (minus:borrow: e2 f2 1)) 144)


(divmod:with: (new: Natural 1)
              1
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 15)
              4
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 9)
              7
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 16)
              16
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 256)
              16
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 0)
              1
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(divmod:with: (new: Natural 17)
              256
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline)))

(check-error (divmod:with: (new: Natural 20)
              -21
              (block (q r) (print (toInt q))
                           (print space)
                           (print (toInt r))
                           (print newline))))


(check-expect
  (decimal (new Natural))
  '(0))

(check-expect
  (decimal (new: Natural 1))
  '(1))

(check-expect
  (decimal (new: Natural 45))
  '(4 5))

(check-expect
  (decimal (new: Natural 255))
  '(2 5 5))

(check-expect
  (decimal (new: Natural 4096))
  '(4 0 9 6))

(check-expect
  (decimal
    (new: Natural 100))
  '(1 0 0))

(check-expect
  (decimal
    (* (new: Natural 100) (new: Natural 2)))
  '(2 0 0))

(check-expect
  (decimal
    (* (new: Natural 100) (new: Natural 10)))
  '(1 0 0 0))

(check-expect
  (decimal
    (* (new: Natural 100) (new: Natural 100)))
  '(1 0 0 0 0))
