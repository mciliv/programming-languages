;; Comp105 scheme assignment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 30
;;
;;
;; a) vector-length returns the square root of (x^2 + y^2)
;;
;; b) It works by using the regular "let" function which has set
;; the multiplication symbol as addition and vice versa. "let" can do this
;; because it evaluates all "e'"s fist then sets the names equal to those
;; expressions.






;; You will implement these functions.
(check-assert (procedure? count))
(check-assert (procedure? countall))
(check-assert (procedure? mirror))
(check-assert (procedure? flatten))
(check-assert (procedure? contig-sublist?))
(check-assert (procedure? sublist?))
(check-assert (procedure? takewhile))
(check-assert (procedure? dropwhile))
(check-assert (procedure? take))
(check-assert (procedure? drop))
(check-assert (procedure? zip))
(check-assert (procedure? unzip))
(check-assert (procedure? arg-max))
(check-assert (procedure? maximally-distant-point))
(check-assert (procedure? merge))
(check-assert (procedure? interleave))
(check-assert (procedure? remove-one-copy))
(check-assert (procedure? permutation?))
(check-assert (procedure? split-list))

;; note to staff: template headers generated by `template-header`


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2a
;;
;; (count x xs) returns the number of (top-level) elements of xs equal to x
;; when x is an atom and xs is in LIST(SEXP)
;; 
;; 
;; laws:
;;
;;   (count x '())         == 0
;;   (count x (cons x ys)) == (+ 1 (count x ys))
;;   (count x (cons y ys)) == (count x ys)

(check-expect (count 'a '()) 0)
(check-expect (count 'a (cons 'a '(b c))) 1)
(check-expect (count 'a (cons 'b '(a c))) 1)


(define count (x xs)
  (if (null? xs)
    0
    (if (= x (car xs))
      (+ 1 (count x (cdr xs)))
      (count x (cdr xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2b
;;
;; (countall x xs) returns the number of times x occurs anywhere in xs, not
;; only at the top level, when x is an atom and xs is in LIST(SEXP)
;; 
;; laws: 
;;   (countall x '())         == 0
;;   (countall x (cons x ys)) == (+ 1 (countall x ys))
;;   (countall x (cons y ys)) == (countall x ys)
;;   (countall x (cons xs ys)) == (countall x xs) + (countall x ys)
;;    

(check-expect (countall 'a '()) 0)
(check-expect (countall 'a (cons 'a '(b c))) 1)
(check-expect (countall 'a (cons 'b '(a c))) 1)
(check-expect (countall 'a '(cons '((a b) c))) 1)


(define countall (x xs)
  (if (null? xs)
    0
    (if (= x (car xs))
      (+ 1 (countall x (cdr xs)))
      (if (atom? (car xs))
        (countall x (cdr xs))
        (countall x (car xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2c
;;
;; (mirror xs) returns a list in which every list in xs is recusively
;; mirrored, and the resulting lists are in reverse order.
;; 
;; laws:
;;  (mirror '()) == '()
;;  (mirror (cons x xs) == (append (mirror xs) (list1 x)))
;;  (mirror (cons xs ys) == (append (mirror ys) (list1 (mirror xs)))
;;

(check-expect (mirror '()) '())
(check-expect (mirror (cons 'a '(b c))) '(c b a))
(check-expect (mirror (cons '(a b) '(c d))) '(d c (b a)))

(define mirror (xs)
  (if (null? xs)
    '()
    (if (atom? (car xs))
      (append (mirror (cdr xs)) (list1 (car xs))) 
      (append (mirror (cdr xs)) (list1 (mirror (car xs))))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2d
;;
;; (flatten xs) constructs a list having the same atoms as xs in the same
;; order, but in a flat list, erasing internal parenthesis.
;; 
;; laws:
;;  (flatten '()) == '()
;;  (flatten xs) == xs
;;  (flatten (cons xs ys)) == (append xs ys)
;;

(check-expect (flatten '()) '())
(check-expect (flatten '(a b)) '(a b))
(check-expect (flatten '(a (b c))) '(a b c))

(define flatten (xs)
  (if (null? xs)
    '()
    (if (atom? (car xs))
      (cons (car xs) (flatten (cdr xs)))
      (append (flatten (car xs)) (flatten(cdr xs))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2e
;;
;; (contig-sublist xs ys) returns #t if and only if there are two other lists
;; "front" and "back", such that ys is equal to
;; (append (append front xs) back), meaning xs is a contiguous subsequence of
;; the list ys.
;;
;; For `contig-sublist?`, algebraic laws are optional.
;; But as always, laws for helper functions are mandatory.

(check-expect (contig-sublist? '() '()) #t)
(check-expect (contig-sublist? '(b c) '(a b b c d)) #t)

(define contig-sublist? (xs ys)
  (if (null? xs)
    #t
    (if (null? ys)
      #f
      (if (= (car xs) (car ys))
        (if (contig-sublist? (cdr xs) (cdr ys))
          #t
          (contig-sublist? xs (cdr ys)))
        (contig-sublist? xs (cdr ys))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2f
;;
;; (sublist? xs ys) returns #t if and only if the list ys contains the elements
;; of xs, in the same order, but possibly with other values in between.
;;
;; laws:
;;  (sublist? '() '()) == #t
;;  (sublist? (append xs ys) (append as (append xs (append bs (append ys cs))))
;;  == #t
;;  (sublist? (append xs ys) (append ys xs)) == #f
;;  (sublist? (append xs ys) (append as bs)) ==
;;  (and (sublist? xs as) (sublist? ys bs))

(check-expect (sublist? '() '()) #t)
(check-expect (sublist? '(b d) '(a b c d e)) #t)
(check-expect (sublist? '(a b) '(b a)) #f)
(check-expect (sublist? '(a c e) '(a b c d e)) #t)

(define sublist? (xs ys)
  (if (null? xs)
    #t
    (if (null? ys)
      #f
      (if (= (car xs) (car ys))
        (sublist? (cdr xs) (cdr ys))
        (sublist? xs (cdr ys))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 10
;;
;; (takewhile p? xs) returns the longest prefix of the list in which every
;; element satisfies the predicate.
;;
;; (takewhile p? '()) == '()
;; (takewhile p? (cons p xs)) == (cons p (takewhile p? xs))
;; (takewhile p? (cons notP xs)) == '()

(define even? (x) (= (mod x 2) 0))

(check-expect (takewhile even? '()) '())
(check-expect (takewhile even? '(0 2 3 4)) '(0 2))

(define takewhile (p? xs)
  (if (null? xs)
    '()
    (if (p? (car xs))
      (cons (car xs) (takewhile p? (cdr xs)))
      '())))



;; (dropwhile p? xs) returns whatever is leftover after removing the longest
;; prefix in which every element satisfies the predicate.
;;
;; (dropwhile p? '()) == '()
;; (dropwhile p? (cons notP xs)) == (cons notP xs)
;; (dropwhile p? (cons p xs)) == (dropwhile p? xs)


(check-expect (dropwhile even? '()) '())
(check-expect (dropwhile even? '(1 0)) '(1 0))
(check-expect (dropwhile even? '(0 1 2)) '(1 2))

(define dropwhile (p? xs)
  (if (null? (takewhile p? xs))
    xs
    (if (= xs (takewhile p? xs))
      '()
      (dropwhile p? (cdr xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem B (take)
;;
;;(take n xs) expects a natural number and a list. It returns the longest
;; prefix of xs that contains at most n elements.
;;
;; laws:
;;  (take 0 xs) == '()
;;  (take n '()) == '()
;;  (take n (cons x xs)) == (cons x (take (- n 1) xs))

(check-expect (take 0 '(a)) '())
(check-expect (take 1 '()) '())
(check-expect (take 1 '(a b)) '(a))
(check-expect (take 3 '(a b)) '(a b))


(define take (n xs)
  (if (or (= n 0) (null? xs))
    '()
    (cons (car xs) (take (- n 1) (cdr xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem B (drop)
;;
;; (drop n xs) expects a natural number and a list. It returns the list with
;; n elements removed from the front of the list.
;;
;; laws:
;;  (drop 0 xs) == xs
;;  (drop n '()) == '()
;;  (drop n xs) == (drop (- n 1) (cdr xs))

(check-expect (drop 0 '(a)) '(a))
(check-expect (drop 1 '()) '())
(check-expect (drop 1 '(a b)) '(b))

(define drop (n xs)
  (if (= n 0)
    xs
    (if (null? xs)
      '()
      (drop (- n 1) (cdr xs)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem C (zip)
;;
;; (zip xs ys) coverts the pair of lists to an association list.
;;
;; laws:
;;  (zip '() '()) == '()
;;  (zip '(cons x xs) (cons y ys)) == (cons (x y) (zip xs ys))
;;

(check-expect (zip '() '()) '())
(check-expect (zip '(a b) '(1 2)) '((a 1) (b 2)))


(define zip (xs ys)
  (if (null? xs)
    '()
    (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem C (unzip)
;;
;; (unzip xs) coverts the association list to a pair of lists. 
;;
;; For `unzip`, algebraic laws are optional.

(check-expect (unzip '()) '(() ()))
(check-expect (unzip '((a 1) (b 2) (c 3))) '((a b c) (1 2 3)))

(define unzip (xs)
  (if (null? xs)
    (list2 '() '())
    (list2
      (cons (car (car xs)) (car (unzip (cdr xs))))
      (cons (cadr (car xs)) (cadr (unzip (cdr xs)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem D
;;
;; (arg-max f as) returns an element "a" in "as" for which (f a) is as
;; large as possible. It expects a function f that maps a value in set A to a
;; number and a non-empty list "as" of values in set A. 
;;
;; laws:
;;  (arg-max f '(a)) == a
;;  (arg-max f (cons a_fmax as)) == a_fmax
;;  (arg-max f (cons a_notfmax as)) == (arg-max f as)

(define square (x) (* x x))

(check-expect (arg-max square '(2)) 2)
(check-expect (arg-max square '(2 1)) 2)

(check-expect (arg-max length '((a))) '(a))
(check-expect (arg-max length '((a b) (a))) '(a b))

(define arg-max (f as)
  (if (= (length as) 1)
    (car as)
    (if (> (f (car as)) (f (arg-max f (cdr as))))
      (car as)
      (arg-max f (cdr as)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem E
;;
;; (maximally-distant-point ps) takes a nonempty List of point records and
;;  returns the one that is most distant from the main diagonal.
;;
;; For this problem, algebraic laws are optional, even for helper functions.
;;

(record point (x y))

;; laws:  (abs n) == (- 0 n), when n < 0
;;        (abs n) == n,       when n >= 0
(check-expect (abs -12) 12)
(check-expect (abs 3)    3)

;; absolute value
(define abs (n) 
  (if (< n 0) (- 0 n) n))

(check-expect (distant-point (make-point 3 4)) 1)
(check-expect (distant-point (make-point 4 3)) 1)

;; distance of point from diagonal
(define distant-point (p)
  (abs (- (point-y p) (point-x p))))

(check-expect (maximally-distant-point '((make-point 3 4))) (make-point 3 4))
(check-expect (maximally-distant-point '((make-point 3 4) (make-point 3 5)))
  (make-point 3 5))

(define maximally-distant-point (ps)
    (arg-max distant-point ps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem F
;;
;; (merge xs ys) returns a single sorted list in increasing order from the two
;; sorted lists which are in increasing order.
;;
;; (merge '() ys) == ys
;; (merge xs '()) == xs
;; (merge (cons small xs) '(big ys)) == (cons small (merge xs (cons big ys)))
;; (merge (cons small xs) '(big ys)) == (cons small (merge xs (cons big ys)))

(check-expect (merge '() '()) '())
(check-expect (merge '() '(1)) '(1))
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '(1 4) '(2 3)) '(1 2 3 4))


(define merge (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (if (< (car xs) (car ys))
        (cons (car xs) (merge (cdr xs) ys))
        (cons (car ys) (merge xs (cdr ys)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem G
;;
;; (interleave xs ys) returns a single list obtained by choosing elements
;; alternately, first from xs and then from ys. When either xs or ys runs out,
;; interleave takes the remaining elements from the other list, so that the
;; elements of the result are exactly the elements of the two argument lists
;; taken together.
;;
;; (interleave '() ys) == ys
;; (interleave xs '()) == xs
;; (interleave (cons x xs) (cons y ys)) == (cons x (cons y (interleave xs ys)))

(check-expect (interleave '() '(a)) '(a))
(check-expect (interleave '(a) '()) '(a))
(check-expect (interleave '(a c) '(b d e)) '(a b c d e))


(define interleave (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (cons (car xs) (cons (car ys) (interleave (cdr xs) (cdr ys)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem H
;;
;;
;;
;; laws:
;;  (remove-one-copy x '()) == (error removed-an-absent-item)
;;  (remove-one-copy x '(x)) == '()
;;  (remove-one-copy x (cons x xs)) == xs
;;  (remove-one-copy x (cons y xs)) == (cons y (remove-one-copy x xs))

(check-error (remove-one-copy 1 '()))
(check-expect (remove-one-copy 1 '(1)) '())
(check-expect (remove-one-copy 1 '(1 2)) '(2))
(check-expect (remove-one-copy 2 '(1 2)) '(1))

(define remove-one-copy (s xs)
  (if (null? xs)
    (error removed-an-absent-item)
    (if (= (car xs) s)
      (cdr xs)
      (cons (car xs) (remove-one-copy s (cdr xs))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem I
;;
;; (permutation? xs ys) returns whether xs and ys are permutations.
;;
;; laws:
;;  (permutation? '() '()) == #t
;;  (permutation? '(x) '()) == #f
;;  (permutation? '() '(x)) == #f
;;  (permutation? '(x) '(y)) == #f
;;  (permutation? '()
;;

;; <tests>

(define permutation? (xs ys)
  ((and (null? xs) (null? ys))
    #t
    (if (null? xs)
      #f)
      (if (= (count (car xs) ys))
        (permutation? (cdr xs)))))
