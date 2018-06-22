val () = Unit.checkAssert "lambda + app string"
           (fn () =>
              (toString (lam "x" (app (var "x") (var "y")))
                  = "(lambda (x) (x y))"))
val () = Unit.checkAssert "fst string"
           (fn () =>
               (toString (app (app (var "fst") (var "p")) (var "x"))
                  = "((fst p) x)"))
val () = Unit.checkAssert "nested lambda string"
           (fn () =>
              (toString (app (app (lam "x" (lam "y" (var "x")))
                        (lam "z" (var "a"))) (var "b")) =
                            "(((lambda (x) (lambda (y) x)) (lambda (z) a)) b)"))