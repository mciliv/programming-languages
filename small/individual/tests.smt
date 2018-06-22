(check-expect (+ (num:den: Fraction 1 2) 2) (num:den: Fraction 5 2))
(check-expect (- (num:den: Fraction 13 4) 3) (num:den: Fraction 1 4))
(check-expect (* (num:den: Fraction 5 4) 3) (num:den: Fraction 15 4))