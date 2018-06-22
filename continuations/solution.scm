;; Comp105 Continuations Assignment

(check-assert (procedure? formula?))
(check-assert (procedure? eval-formula))
(check-assert (procedure? find-formula-true-asst))

(record not [arg])
(record or  [args])
(record and [args])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem F: Recognizing forumulas
;;
;; (formula? s) returns #t if the S-expression represents a Boolean formula
;; and #f otherwise
;;
;; Laws:
;;  (formula? symbol) == #t
;;  (formula? not-record) == (formula? (not-arg not-record))
;;  (formula? or-record) == (all? formula? or-args)
;;  (formula? and-record) == (all? formula? and-args)
;;  When none of the laws above apply, return #f

(check-assert (formula? 'a))
(check-assert (formula? (make-not 'a)))
(check-assert (formula? (make-or '(a b))))
(check-assert (formula? (make-and '(a b))))
(check-assert (not (formula? 1)))

(define formula? (s)
  (if (symbol? s)
    #t
    (if (not? s)
      (formula? (not-arg s))
      (if (or? s)
        (all? formula? (or-args s))
        (if (and? s)
          (all? formula? (and-args s))
          #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem E: Evaluating formulas
;;
;; (eval-formula f env) returns #t if the formula, f, is satisfied in the given
;; environment, e. Otherwise, returns #f.
;;
;; Laws:
;;  (eval-formula symbol env) == (find symbol env)
;;  (eval-formula not-record) == (not (eval-formula (not-arg not-record)))
;;  (eval-formula or-record) ==
;;    (exists? (lambda (x) (= (eval-formula x) #t)) (or-args or-record))
;;  (eval-formula and-record) ==
;;    (all? (lambda (x) (= (eval-formula x) #t)) (and-args and-record))


(check-assert (eval-formula 'a '([a #t] [b #f])))
(check-assert (not (eval-formula 'b '([a #t] [b #f]))))
(check-assert (eval-formula (make-or '(a b)) '([a #t] [b #f])))
(check-assert (not (eval-formula (make-and '(a b)) '([a #t] [b #f]))))

(define eval-formula (f env)
  (if (symbol? f)
    (find f env)
    (if (not? f)
      (not (eval-formula (not-arg f) env))
      (let ([true? (lambda (x) (= (eval-formula x env) #t))])
        (if (or? f)
          (exists? true? (or-args f))
          (all? true? (and-args f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem S: SAT solving using continuation-passing style
;;
;; (find-formula-true-asst f fail succeed) searches for an assignment that
;; satisfies formula f. If it finds a satisfying assignment, it calls "succ",
;; passing both the satisfying assignment (as an association list) and a resume
;; continuation. If it fails to find a satisfying assignment, it calls "fail".
;;
;; Laws:
;;  (find-formula-asst x bool cur fail succeed) ==
;;    (find-formula-symbol x bool cur fail succeed), where x is a symbol
;;  (find-formula-asst (make-not f) bool cur fail succeed) ==
;;    (find-formula-asst (not-arg f) (not bool) cur fail succeed) 
;;  (find-formula-asst (make-or  fs) #t   cur fail succeed) ==
;;    (find-any-asst fs #t cur fail succeed)
;;  (find-formula-asst (make-or  fs) #f   cur fail succeed) ==
;;    (find-any-asst fs #f cur fail succeed)
;;  (find-formula-asst (make-and fs) #t   cur fail succeed) ==
;;    (find-all-asst fs #t cur fail succeed)
;;  (find-formula-asst (make-and fs) #f   cur fail succeed) ==
;;    (find-all-asst fs #f cur fail succeed)
;;
;;  (find-all-asst '()         bool cur fail succeed) ==
;;    (succeed cur fail)   
;;  (find-all-asst (cons f fs) bool cur fail succeed) ==
;;    (find-formula-asst f bool cur fail (find-all-asst fs bool cur fail
;;    succeed))
;;  (find-any-asst '()         bool cur fail succeed) ==
;;    (fail)
;;  (find-any-asst (cons f fs) bool cur fail succeed) ==
;;    (find-formula-asst f bool cur (find-any-asst fs bool cur fail succeed)
;;    succeed)
;;
;;  (find-formula-symbol x bool cur fail succeed) ==
;;    (succeed (bind x bool cur) fail), where x is not bound in cur
;;  (find-formula-symbol x bool cur fail succeed) ==
;;    (succeed cur fail), where x is bool in cur
;;  (find-formula-symbol x bool cur fail succeed) ==
;;    (fail), where x is (not bool) in cur
;;
;;   (find-formula-true-asst f fail succeed) ==
;;     (find-formula-asst f #t '() fail succeed)

(define find-formula-true-asst (f fail succeed)
  (letrec
    ([find-formula-asst
      (lambda (formula bool cur fail succeed)
        (if (symbol? formula)
          (find-formula-symbol formula bool cur fail succeed)
          (if (not? formula)
            (find-formula-asst (not-arg formula) (not bool) cur fail succeed)
            (if (or? formula)
              (find-any-asst (or-args formula) bool cur fail succeed)
              (if (and? formula)
                (find-all-asst (and-args formula) bool cur fail succeed)
                (fail))))))]

     [find-all-asst
      (lambda (formulas bool cur fail succeed)
        (if (null? formulas)
          (succeed cur fail)
          (find-formula-asst
            (car formulas)
            bool
            cur
            fail
            (lambda (cur resume)
              (find-all-asst (cdr formulas) bool cur resume succeed)))))]

     [find-any-asst
      (lambda (formulas bool cur fail succeed)
        (if (null? formulas)
          (fail)
          (find-formula-asst
            (car formulas)
            bool
            cur
            (lambda ()
              (find-any-asst (cdr formulas) bool cur fail succeed))
            succeed)))]

     [find-formula-symbol
      (lambda (x bool cur fail succeed)
        (if (equal? bool (find x cur))
          (succeed cur fail)
          (if (equal? (not bool) (find x cur))
            (fail)
            (succeed (bind x bool cur) fail))))])

    (find-formula-asst f #t '() fail succeed)))

(use solver-interface-tests.scm)
(use solver-tests.scm)

(check-expect
  (find-formula-true-asst f1
        (lambda () 'no-solution)
        (lambda (cur resume) cur))
  s1)
  
(check-expect
  (find-formula-true-asst f2
        (lambda () 'no-solution)
        (lambda (cur resume) cur))
  s2)

(check-expect
  (find-formula-true-asst f3
        (lambda () 'no-solution)
        (lambda (cur resume) cur))
  s3)

(check-expect
  (find-formula-true-asst f4
        (lambda () 'no-solution)
        (lambda (cur resume) cur))
  s4)


