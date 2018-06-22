;; Comp105 Continuations Assignment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem T: SAT Solver Test Cases 
;;

(record not [arg])   ;; OK if these are duplicates
(record or  [args])
(record and [args])

; Test not
(val f1 (make-not 'a))
(val s1 '([a #f]))

; Tests a complex statement with more than one satisfying assignment
(val f2 (make-and (list2
                    (make-or (list2 'a (make-not 'b)))
                    (make-or (list2 (make-not 'a) 'b)))))
(val s2 '([a #t] [b #t]))

; Tests a statement with no solution
(val f3  (make-and (list2 'a (make-not 'a))))
(val s3 'no-solution)

; Tests just a symbol
(val f4 'a)
(val s4 '([a #t]))
