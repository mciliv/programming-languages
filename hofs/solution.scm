;; Comp105 hofs assignment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 14b
;;
;; (max* xs) finds the maximum of a non-empty list of integers. Function max
;; does not have a left or right identity.

(check-expect (max* '(1)) 1)
(check-expect (max* '(1 1)) 1)
(check-expect (max* '(1 2)) 2)
(check-expect (max* '(2 1)) 2)

(define max* (xs)
  (foldl max (car xs) (cdr xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 14f
;;
;; (product xs) finds the product of a non-empty list of integers.

(check-expect (product '(1)) 1)
(check-expect (product '(-2)) -2)
(check-expect (product '(1 -2 3)) -6)

(define product (xs)
  (foldl * (car xs) (cdr xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 14h
;;
;; (append xs ys) appends the 2 lists.

(check-expect (append '() '()) '())
(check-expect (append '(1) '()) '(1))
(check-expect (append '() '(1)) '(1))
(check-expect (append '(1) '(2)) '(1 2))

(define append (xs ys)
  (foldl cons ys xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 14j
;;
;; (reverse xs) reverses the list.

(check-expect (reverse '()) '())
(check-expect (reverse '(1)) '(1))
(check-expect (reverse '(1 2)) '(2 1))
(check-expect (reverse '(1 3 2)) '(2 3 1))

(define reverse (xs)
  (foldl cons '() xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 15: length
;;
;; (length xs) is the length of the list.

(check-expect (length '()) 0)
(check-expect (length '(1)) 1)
(check-expect (length '(1 2)) 2)
(check-expect (length '(1 2 -1)) 3)

(define length (xs)
  (foldl (lambda (x counter) (+ counter 1)) 0 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 15: map
;;
;; (map f xs) returns the list with f applied to every member of the list.


(define multiply-by-2 (x) (* x 2))

(check-expect (map multiply-by-2 '()) '())
(check-expect (map multiply-by-2 '(1)) '(2))
(check-expect (map multiply-by-2 '(-1 0 3)) '(-2 0 6))

(define map (f xs)
  (foldr (lambda (x new_list) (cons (f x) new_list)) '() xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 15: filter
;;
;; (filter p? xs) returns a new list sonsisting of only those elements of xs
;; that satisfy p?

(define even? (x) (= (mod x 2) 0))

(check-expect (filter even? '()) '())
(check-expect (filter even? '(0)) '(0))
(check-expect (filter even? '(1)) '())
(check-expect (filter even? '(0 1 2 3)) '(0 2))

(define filter (p? xs)
  (foldr
    (lambda (x new_list)
      (if (p? x) (cons x new_list) new_list))
    '()
    xs))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 15: exists?
;;
;; (exists? p? xs) tells whether there is an element of the list satisfying the
;; predicate

(check-expect (exists? even? '()) #f)
(check-expect (exists? even? '(0)) #t)
(check-expect (exists? even? '(1)) #f)
(check-expect (exists? even? '(0 1 2 3)) #t)
(check-expect (exists? even? '(1 2 3 4)) #t)

(define exists? (p? xs)
  (foldl
    (lambda (x exists)
      (or (p? x) exists))
    #f
    xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 15: all?
;;
;; (all? p? xs) tells whether all elements of the list satisfy the predicate

(check-expect (all? even? '()) #t)
(check-expect (all? even? '(0)) #t)
(check-expect (all? even? '(1)) #f)
(check-expect (all? even? '(0 2)) #t)
(check-expect (all? even? '(0 1 2)) #f)

(define all? (p? xs)
  (foldl
    (lambda (x all)
      (and (p? x) all))
    #t
    xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 19a
;;
;; (member? x evens) returns true if x is in evens, which is the set of all
;; even integers.

(check-expect (member? 0 evens) #t)
(check-expect (member? -1 evens) #f)
(check-expect (member? -2 evens) #t)
(check-expect (member? 3 evens) #f)
(check-expect (member? 4 evens) #t)

(define member? (x s) (s x))

(val evens even?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 19b
;;
;; (member? x two-digits) returns true if x is in two-digits, which is the set
;; of all two-digit positive numberts.

(check-expect (member? -99 two-digits) #f)
(check-expect (member? -1 two-digits) #f)
(check-expect (member? 9 two-digits) #f)
(check-expect (member? 10 two-digits) #t)
(check-expect (member? 56 two-digits) #t)
(check-expect (member? 99 two-digits) #t)
(check-expect (member? 100 two-digits) #f)

(val two-digits (lambda (x) (and (> x 9) (< x 100))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 19c
;;
;; (member? x (add-element x s)) == #t
;; (member? x (add-element y s)) == (member? x s), where (not (equal? y x))
;; (member? x (union s1 s2))     == (or (member? x s1) (member? x s2))
;; (member? x (inter s1 s2))     == (and (member? x s1) (member? x s2))
;; (member? x (diff  s1 s2))     == (and (member? x s1) (not (member? x s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (add-element to_add s) adds the element to the set.
;;

(check-expect (member? 0 emptyset) #f)
(check-expect (member? 1 emptyset) #f)
(check-expect (member? -2 emptyset) #f)

(val emptyset (lambda (x) #f))

(check-expect (member? 0 (add-element 0 emptyset)) #t)
(check-expect (member? 1 (add-element 1 evens)) #t)
(check-expect (member? 0 (add-element 1 evens)) #t)
(check-expect (member? 3 (add-element 1 evens)) #f)

(define add-element (to_add s)
  (lambda (x) (or (equal? x to_add) (s x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (union s1 s2) returns the union of the 2 lists.
;;

(check-expect (member? 1 (union evens two-digits)) #f)
(check-expect (member? 2 (union evens two-digits)) #t)
(check-expect (member? 11 (union evens two-digits)) #t)
(check-expect (member? 12 (union evens two-digits)) #t)

(define union (s1 s2)
  (lambda (x) (or (s1 x) (s2 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (inter s1 s2) returns the intersection of the 2 lists.
;;

(check-expect (member? 1 (inter evens two-digits)) #f)
(check-expect (member? 2 (inter evens two-digits)) #f)
(check-expect (member? 11 (inter evens two-digits)) #f)
(check-expect (member? 12 (inter evens two-digits)) #t)

(define inter (s1 s2)
  (lambda (x) (and (s1 x) (s2 x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (diff s1 s2) returns the set that contains every element of s1 that is not
;; also in s2
;;

(check-expect (member? 1 (diff evens two-digits)) #f)
(check-expect (member? 2 (diff evens two-digits)) #t)
(check-expect (member? 11 (diff evens two-digits)) #f)
(check-expect (member? 12 (diff evens two-digits)) #f)

(define diff (s1 s2)
  (lambda (x) (and (s1 x) (not (s2 x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 19d
;;
;; Implements the third approach to polymorphism


(record set-ops (emptyset member? add-element union inter diff))

(define set-ops-from (eq?)
  (let ([emptyset   (lambda (x) #f)]
        [member? (lambda (x s) (s x))]
        [add-element
          (lambda (to_add s)
            (lambda (x) (or (eq? x to_add) (s x))))]
        [union   (lambda (s1 s2) (lambda (x) (or (s1 x) (s2 x))))]
        [inter   (lambda (s1 s2) (lambda (x) (and (s1 x) (s2 x))))]
        [diff    (lambda (s1 s2) (lambda (x) (and (s1 x) (not (s2 x)))))])
    (make-set-ops emptyset member? add-element union inter diff)))

(val atom-set-ops (set-ops-from =))
(val emptyset      (set-ops-emptyset atom-set-ops))
(val member?      (set-ops-member? atom-set-ops))
(val add-element  (set-ops-add-element atom-set-ops)) 
(val union        (set-ops-union atom-set-ops))
(val inter        (set-ops-inter atom-set-ops))
(val diff         (set-ops-diff atom-set-ops))

(check-assert (procedure? set-ops-from))
(check-assert (set-ops? (set-ops-from =)))

(check-expect (member? 0 emptyset) #f)
(check-expect (member? 1 emptyset) #f)
(check-expect (member? -2 emptyset) #f)

(check-expect (member? 0 evens) #t)
(check-expect (member? -1 evens) #f)
(check-expect (member? -2 evens) #t)
(check-expect (member? 3 evens) #f)
(check-expect (member? 4 evens) #t)

(check-expect (member? 0 (add-element 0 emptyset)) #t)
(check-expect (member? 1 (add-element 1 evens)) #t)
(check-expect (member? 0 (add-element 1 evens)) #t)
(check-expect (member? 3 (add-element 1 evens)) #f)

(check-expect (member? 1 (union evens two-digits)) #f)
(check-expect (member? 2 (union evens two-digits)) #t)
(check-expect (member? 11 (union evens two-digits)) #t)
(check-expect (member? 12 (union evens two-digits)) #t)

(check-expect (member? 1 (inter evens two-digits)) #f)
(check-expect (member? 2 (inter evens two-digits)) #f)
(check-expect (member? 11 (inter evens two-digits)) #f)
(check-expect (member? 12 (inter evens two-digits)) #t)

(check-expect (member? 1 (diff evens two-digits)) #f)
(check-expect (member? 2 (diff evens two-digits)) #t)
(check-expect (member? 11 (diff evens two-digits)) #f)
(check-expect (member? 12 (diff evens two-digits)) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem A
;;
;; (f-functional (y) (locals x)) returns the same thing as the following
;; imperative Impcore-with-locals function:
;;
;; (define f-imperative (y) (locals x)
;;   (begin 
;;     (set x e)
;;     (while (p? x y)
;;       (set x (g x y)))
;;     (h x y)))

(define f-functional (y)
  (let ([x e]
        [f-helper (lambda (x) (if (p? x y) (f-helper (g x y)) x))])
        (h (f-helper x) y)))


;; Permutation code from
;; https://www.cs.tufts.edu/comp/105/solutions/scheme.remove+permutation
;;
;;(remove-one-copy x xs) returns xs with one element 'equal?' to x removed,
;; where xs has at least one instance of x, causes checked run-time error if
;; not 
;;
;; laws: (remove-one-copy x (cons x xs)) == xs
;;       (remove-one-copy x (cons y ys)) = (cons y (remove-one-copy x ys)), 
;;                              when x is different from y

(check-expect (remove-one-copy 1 '(1 2 3 4)) '(2 3 4))
(check-expect (remove-one-copy 1 '(2 1 3 4)) '(2 3 4))

(define remove-one-copy (x xs)
  (if (null? xs)
      (error 'removed-an-absent-item)
      (if (equal? x (car xs))
          (cdr xs)
          (cons (car xs) (remove-one-copy x (cdr xs))))))

(check-error  (remove-one-copy 'a '()))
(check-expect (remove-one-copy 'b '(a b c)) '(a c))
(check-expect (remove-one-copy 'b '(a b b c)) ; OK whichever copy is removed
              '(a b c)) 
(check-error  (remove-one-copy 'z '(a b c)))

;;(permutation? xs ys) returns a Boolean saying if xs is a permutation of ys,
;;where xs and ys are lists 
;;
;; laws:
;;   (permutation? '() '())         == #t
;;   (permutation? '() (cons z zs)) == #f
;;   (permutation? (cons z zs)  ys) == (permutation? zs (remove-one-copy z ys))
;;      when (member? z ys)
;;   (permutation? (cons z zs)  ys) == #f      
;;              when (not (member? z ys))
;;
(check-assert      (permutation? '() '()))
(check-assert (not (permutation? '() '(1 2 3)))) 
(check-assert      (permutation? '(1 2 3)  '(2 1 3)))    ; member, true
(check-assert (not (permutation? '(1 2 3)  '(2 1 3 4)))) ; member, false
(check-assert (not (permutation? '(1 2 3) '(2 3))))      ; nonmember

(define permutation? (xs ys)
  (if (null? xs)
      (null? ys)
      (let ([z  (car xs)]
            [zs (cdr xs)]
            [member? (lambda (a as) (exists? ((curry equal?) a) as))])
        (if (member? z ys)
            (permutation? zs (remove-one-copy z ys))
            #f))))

(check-assert      (permutation? '(a b c) '(b c a)))
(check-assert (not (permutation? '(a b c) '(a a b c))))
(check-assert (not (permutation? '(a b a) '(b a b))))
(check-assert (not (permutation? '(a b c) '(a b c d e f g))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem G1
;;
;; (edge-list g) returns a list of all the edges in the graph g.

(record edge [from to])

(check-assert (permutation? (edge-list '()) '()))
(check-assert (permutation? (edge-list '([A ()] [B ()])) '()))
(check-assert
  (permutation? (edge-list '([A (B C)] [B (C)] [C ()]))
                (list3 (make-edge 'A 'B) (make-edge 'A 'C) (make-edge 'B 'C))))
(check-assert
    (permutation?
      (edge-list '([A (B C)] [B (A C)] [C (A B)]))
      (list6
        (make-edge 'A 'B)
        (make-edge 'A 'C)
        (make-edge 'B 'A)
        (make-edge 'B 'C)
        (make-edge 'C 'A)
        (make-edge 'C 'B))))

;; use let, lambda, and either of the fold functions
(define edge-list (g)
  (foldr
    append
    '()
    (map
      (lambda (n)
        (map (lambda (attached_to) (make-edge (car n) attached_to)) (cadr n)))
      g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem G2
;;
;; (add-edge e g) takes two arguments: an edge made with make-edge and a graph
;; that is represented as a successors map. It returns a new graph that is like
;; the original, except that the new graph has had the given edge added to it.
;;
;; a law:
;;   (permutation? (cons e (edge-list g))
;;                 (edge-list (add-edge e g)))

(check-assert
  (permutation?
    (cons (make-edge 'A 'B) (edge-list '()))
    (edge-list (add-edge (make-edge 'A 'B) '()))))
(check-expect
  (add-edge (make-edge 'A 'B) '())
  '([A (B)]))
(check-expect
  (add-edge (make-edge 'A 'B) '([B (A)]))
  '([B (A)] [A (B)]))

(define add-edge (e g)
  (if (null? (find (edge-from e) g))
    (bind (edge-from e) (list1 (edge-to e)) g)
    (bind (edge-from e) (cons (edge-to e) (find (edge-from e) g)) g)))


