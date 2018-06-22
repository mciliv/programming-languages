;; Problem TD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ((@ drop 'a) n xs) expects a natural number and a 'a list. It returns the
;; list with n elements removed from the front of the list.
;;
;; Laws:
;;  ((@ drop 'a) 0 xs) == xs
;;  ((@ drop 'a) n '()) == '()
;;  ((@ drop 'a) n xs) == (drop (- n 1) (cdr xs))

(check-type ((@ drop int) 2 '(1 2 3)) (list int))
(check-expect ((@ drop sym) 0 '(x)) '(x))
(check-expect ((@ drop sym) 1 (@ '() sym)) (@ '() sym))
(check-expect ((@ drop sym) 1 '(x y)) '(y))

;; Part (a)
(check-type
  drop
  (forall ('a) (int (list 'a) -> (list 'a))))

;; Part (b)
(val drop
  (type-lambda ['a]
    (letrec
      [([drop-mono : (int (list 'a) -> (list 'a))]
        (lambda ([n : int] [xs : (list 'a)])
            (if ((@ = int) n 0)
              xs
              (if ((@ null? 'a) xs)
                (@ '() 'a)
                (drop-mono (- n 1) ((@ cdr 'a) xs))))))]
      drop-mono)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ((@ takewhile 'a) p? xs) returns the longest prefix of the 'a list in which
;; every element satisfies the predicate.
;;
;; Laws:
;;  ((@ takewhile 'a) p? '()) == '()
;;  (takewhile p? (cons notP xs)) == '()
;;  ((@ takewhile 'a) p? ((@ cons 'a) is_p xs)) ==
;;   ((@ cons 'a) is_p (takewhile p? xs))

(check-assert (even? 0))
(check-assert (not (even? 1)))
(check-type (even? 2) bool)

(val even?
  (lambda ([x : int])
    ((@ = int) (mod x 2) 0)))

(check-type ((@ takewhile int) even? '(2)) (list int))
(check-expect ((@ takewhile int) even? (@ '() int)) (@ '() int))
(check-expect ((@ takewhile int) even? '(1 2 3)) (@ '() int))
(check-expect ((@ takewhile int) even? '(0 2 3 4)) '(0 2))

;; Part (c)
(check-type
  takewhile
  (forall ('a) (('a -> bool) (list 'a) -> (list 'a))))

;; Part (d)
(val takewhile
  (type-lambda ['a]
    (letrec
      [([takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
        (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
          (if ((@ null? 'a) xs)
            (@ '() 'a)
            (if (p? ((@ car 'a) xs))
              ((@ cons 'a) ((@ car 'a) xs)
                (takewhile-mono p? ((@ cdr 'a) xs)))
              (@ '() 'a)))))]
      takewhile-mono)))
