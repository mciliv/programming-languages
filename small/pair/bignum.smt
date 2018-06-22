;; TODO: Change return value of divmod remainder

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  LargeInteger Class, given, but with additions

;;;usm.nw:13368
(class LargeInteger Integer
  (magnitude)
  (class-method withMagnitude: (aNatural) 
      (magnitude: (new self) aNatural))
  (method magnitude () magnitude)
  (method magnitude: (aNatural) 
      (set magnitude aNatural)
      self)
  (class-method new: (anInteger)
     (if (negative anInteger) 
        {(magnitude: (new LargeNegativeInteger)
                     (new: Natural (negated anInteger)))}
        {(magnitude: (new LargePositiveInteger) (new: Natural anInteger))}))
  (method asLargeInteger () self)
  (method isZero () (isZero magnitude))
  (method = (anInteger) (isZero   (- self anInteger)))
  (method < (anInteger) (negative (- self anInteger)))

  (method decimal () (locals decimals)
    (set decimals (decimal magnitude))
    (ifTrue: (negative self)
       {(addFirst: decimals ’-)})
    decimals)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Natural Class

(class Natural Magnitude
  [degree digits]

  (class-method new () (new: Natural 0))

  (class-method new: (anInteger)
    (init: (new super) anInteger))

  (method init: (anInteger)
    (set_degree: self anInteger)
    (set_digits: self anInteger)
  self)

  (method set_degree: (anInteger) [locals degree_counter base_multiplier]
    (set degree_counter 0)
    (set base_multiplier (base self))
    (while {(<= base_multiplier anInteger)}
          {(set degree_counter (+ degree_counter 1))
           (set base_multiplier (* base_multiplier (base self)))})
    (set degree degree_counter))

  (method set_digits: (anInteger) [locals remainder index]
    (set digits (new_digits self))
    (set remainder 0)
    (set index 0)
    (while {(<= index degree)}
      {(digit:put: self index (mod: anInteger (base self)))
      (set anInteger (div: (- anInteger (mod: anInteger (base self)))
                                (base self)))
      (set index (+ index 1))}))

  (method copy_degree: (deg)
    (set degree deg))

  (method copy_digits: (aNat) [locals index]
  (set digits (new: Array (+ (degree aNat) 1)))
  (set index 0)
  (while {(<= index (degree aNat))}
    {(at:put: digits index (get_digit: aNat index))
    (set index (+ index 1))}))

  (method new_digits () (new: Array (+ degree 1)))
 
  (method degree () degree)

  (method digits () digits)

  (method base () 16)

  (method digit:put: (index digit) (at:put: digits index digit))

  (method get_digit: (index) (if (| (< index 0) (> index degree))
                              {nil}
                              {(at: digits index)}))

  (method doDigits: (f) [locals deg_temp]
    (set deg_temp degree)
    (while {(> deg_temp 0)}
             {(value (f deg_temp))
              (set deg_temp (- deg_temp 1))}))

  (method divBase () [locals digits_temp index]
    (if (= degree 0)
      {(digit:put: self degree 0)}
      {(set index 0)
      (while {(< index degree)}
             {(digit:put: self index (get_digit: self (+ index 1)))
             (set index (+ index 1))})
      (digit:put: self index 0)
      (trim self)})
    self)

  (method modBase () (get_digit: self 0))

  (method timesBase () [locals index digits_temp]
    (if (isZero self)
      {}
      {(set degree (+ degree 1))
      (set digits_temp (new_digits self))
      (at:put: digits_temp 0 0)
      (set index 1)
      (while {(<= index degree)}
             {(at:put: digits_temp index (get_digit: self (- index 1)))
              (set index (+ index 1))})
      (set digits digits_temp)})
    self)
 
  (method digits_to_trim: (index)
    (if (= (get_digit: self index) 0)
      {(if (> index 0)
      {(+ (digits_to_trim: self (- index 1)) 1)}
      {0})}
    {0}))

  (method trim () [locals index digs_to_trim digits_temp]
    (set digs_to_trim (digits_to_trim: self degree))
    (if (= digs_to_trim 0)
      {} ; no change
      {(set degree (- degree digs_to_trim))
      (set digits_temp (new_digits self))
      (set index 0)
      (while {(<= index degree)}
        {(at:put: digits_temp index (get_digit: self index))
        (set index (+ index 1))})
     (set digits digits_temp)})
  self)

  (method isZero ()
    (& (= degree 0) (= (get_digit: self 0) 0)))

  (method toInt () [locals intval index base_multiplier]
    (set index 0)
    (set intval 0)
    (set base_multiplier 1)
    (while {(<= index degree)}
      {(set intval (+
                      intval
                      (* (get_digit: self index) base_multiplier)))
           (set base_multiplier (* base_multiplier (base self)))
           (set index (+ index 1))})
    intval)

  (method decimal () [locals dec qr quotient remainder]
    (set dec (new List))
    (while {(not (isZero self))}
      {(set qr (block (q r) (set quotient q) (set remainder r)))
      (divmod:with: self 10 qr)
      (addFirst: dec (toInt remainder))
      (set degree (degree quotient))
      (set digits (digits quotient))
      })
    (if (isEmpty dec)
      {(addFirst: dec 0)}
      {dec}))

  (method print ()
    (do: (decimal self) (block (x) (print x))))

  (method printNat () [locals index]
    (set index 0)
    (print left-paren)
    (while {(< index degree)}
      {(print (get_digit: self index))
      (print space)
      (set index (+ index 1))})
   (print (get_digit: self index))
   (print right-paren))

  (method plus:carry: (aNat c) [locals bigdigs smldigs bigdeg smldeg
                                      digits_temp index sum]
    (if (>= degree (degree aNat))
      {(set bigdigs digits)
      (set smldigs (digits aNat))
      (set bigdeg degree)
      (set smldeg (degree aNat))}
      {(set bigdigs (digits aNat))
      (set smldigs digits)
      (set bigdeg (degree aNat))
      (set smldeg degree)})

    (set digits_temp (new: Array (+ bigdeg 2)))
    (set index 0)
    (while {(<= index bigdeg)}
      {(if (< smldeg index)
        {(set sum (+ c (at: bigdigs index)))}
        {(set sum (+ c (+ (at: bigdigs index) (at: smldigs index))))})
      (at:put: digits_temp index (mod: sum (base self)))
      (set c (div: sum (base self)))
      (set index (+ index 1))})

    (at:put: digits_temp index c) ; add 0 or 1 to last index
    (set digits digits_temp)
    (set degree (+ bigdeg 1))
    (trim self)
    self)

  (method minus:borrow: (aNat b) [locals digits_temp result index self_dig
                                        aNat_dig index2]
    (if (< degree (degree aNat))
      {(error: self 'difference-negative)}
      {(set digits_temp (new: Array (+ degree 1)))
      (set index 0)
      (while {(<= index degree)}
        {(set self_dig (get_digit: self index))
         (if (< (degree aNat) index)
           {(set result (- self_dig b))
            (set b 0)}
           {(set aNat_dig (at: (digits aNat) index))
            (if (>= self_dig (+ aNat_dig b))
             {(set result (- self_dig (+ aNat_dig b)))
              (set b 0)}  
             {(set index2 (+ index 1))
              (while {(& (<= index2 degree) 
                         (= (get_digit: self index2) 0))}
                {(set index2 (+ index2 1))})
              (if (> index2 degree) 
                {(error: self 'difference-negative)}
                {(set result (- (+ self_dig 16) (+ aNat_dig b)))
                 (set b 0)
                 (digit:put: self index2 (- (get_digit: self index2) 1))
                 (set index2 (- index2 1))
                 (while {(> index2 index)}
                   {(digit:put: self index2 15)
                    (set index2 (- index2 1))})})})})
         (at:put: digits_temp index result)
         (set index (+ index 1))})})
      (set digits digits_temp)
      (trim self)
      self)

  ; Doesn't change self
  (method divmod:with: (v aBlock) [locals q r divisor incrementor]
    (if (< v 0)
       {(error: self 'result-negative)}
       {(set q (new Natural))
        (set divisor (new: Natural v))
        (set incrementor (new Natural))
        (plus:carry: incrementor divisor 0)

        (while {(compare:withLt:withEq:withGt: incrementor self
               (block () true) (block () true) (block () false))}
          {(set q (+ q (new: Natural 1)))
          (plus:carry: incrementor divisor 0)})
        
        (minus:borrow: incrementor divisor 0)
        (set r (minus:borrow: self incrementor 0))
        (value aBlock q r)}))

  (method + (aNat) (plus:carry: self aNat 0))

  (method - (aNat) (minus:borrow: self aNat 0))

  (method * (aNat) [locals original]
    (set original (new Natural))
    (copy_degree: original degree)
    (copy_digits: original self)
    (while {(not (isZero aNat))}
      {(+ self original)
       (- aNat (new: Natural 1))})
    (- self original)
    self)

  (method div: (divisor) (divmod:with: self divisor (block (q r) q)))

  (method mod: (divisor) (divmod:with: self divisor (block (q r) r)))

  (method = (aNat) (compare:withLt:withEq:withGt: self aNat
                                                  (block () false)
                                                  (block () true)
                                                  (block () false)))

  (method < (aNat) (compare:withLt:withEq:withGt: self aNat
                                                  (block () true)
                                                  (block () false)
                                                  (block () false)))

  (method > (aNat) (compare:withLt:withEq:withGt: self aNat
                                                  (block () false)
                                                  (block () false)
                                                  (block () true)))

  (method compare:withLt:withEq:withGt: (aNat ltBlock eqBlock gtBlock)
    [locals index]
    (if (= degree (degree aNat))
      {(set index degree)
       (while {(& (>= index 0)
                 (= (get_digit: aNat index) (get_digit: self index)))}
        {(set index (- index 1))})
       (if (< index 0)
        {(value eqBlock)}
        {(if (< (get_digit: self index) (get_digit: aNat index))
          {(value ltBlock)}
          {(if (> (get_digit: self index) (get_digit: aNat index))
            {(value gtBlock)}
            {(value eqBlock)})})})}
      {(if (< degree (degree aNat))
        {(value ltBlock)}
        {(if (> degree (degree aNat))
          {(value gtBlock)}
          {(value eqBlock)})})}))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  LargePositiveInteger Class

(class LargePositiveInteger LargeInteger
  []
  (method strictlyPositive () (not (isZero self)))
  
  (method negative () false)

  (method nonnegative () true)

  (method negated () (withMagnitude: LargeNegativeInteger magnitude))
  
  (method + (anInt) (addLargePositiveIntegerTo: (asLargeInteger anInt) self))

  (method addLargePositiveIntegerTo: (anInt)
    (withMagnitude: LargePositiveInteger 
      (+ magnitude (magnitude (asLargeInteger anInt)))))

  (method addLargeNegativeIntegerTo: (anInt)
    (if (>= magnitude (magnitude anInt))
      ; result positive or zero
      {(withMagnitude: LargePositiveInteger (- magnitude (magnitude anInt)))}
      ; result negative 
      {(withMagnitude: LargeNegativeInteger (- (magnitude anInt) magnitude))}))
  
  (method addSmallIntegerTo: (anInt)
    (+ self (asLargeInteger anInt)))

  (method * (anInt) 
    (multiplyByLargePositiveInteger: (asLargeInteger anInt) self))

  (method multiplyByLargePositiveInteger: (anInt)
    (withMagnitude: LargePositiveInteger
      (* magnitude (magnitude anInt))))

  (method multiplyByLargeNegativeInteger: (anInt)
    (withMagnitude: LargeNegativeInteger
      (* magnitude (magnitude anInt))))

  (method multiplyBySmallInteger: (anInt)
    (* self (asLargeInteger anInt)))

  ; (method = (anInt)
  ;   (equalToLargePositiveInteger: anInt self)

  ; (method equalToLargePositiveInteger: (anInt)
  ;   (= magnitude (magnitude anInt)))

  ; (method equalToLargeNegativeInteger: (anInt)
  ;   (and (= magnitude 0) (= (magnitude anInt) 0)))

  (method equalToSmallInteger: (anInt)
    (= self (asLargeInteger anInt)))

  (method smallIntegerLessThan: (anInteger)
    (< self (asLargeInteger anInteger)))

  (method smallIntegerGreaterThan: (anInteger)
    (> self (asLargeInteger anInteger)))

  (method div: (anInt)
    (if (= anInt 0)
      {(error: self 'division-by-zero)}
      {(if (< anInt 0)
        ; result negative
        {(withMagnitude: LargeNegativeInteger          
                  (div: magnitude (magnitude (asLargeInteger anInt))))}
        ; result positive
        {(withMagnitude: LargePositiveInteger 
                 (div: magnitude (magnitude (asLargeInteger anInt))))})}))

  (method print () (printNat magnitude))

  (method smallIntegerGreaterThan: (anInt) (> self (asLargeInteger anInt)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  LargeNegativeInteger Class

(class LargeNegativeInteger LargeInteger
  []
  (method strictlyPositive () false)

  (method negative () true)

  (method nonnegative () false)

  (method negated () (withMagnitude: LargePositiveInteger magnitude))
 
  (method + (anInt) (addLargeNegativeIntegerTo: anInt self))

  (method addLargePositiveIntegerTo: (anInt)
    (if (<= magnitude (magnitude anInt))
      ; result positive or zero 
      {(withMagnitude: LargePositiveInteger (- (magnitude anInt) magnitude))}
      ; result negative
      {(withMagnitude: LargeNegativeInteger (- magnitude (magnitude anInt)))}))

  (method addLargeNegativeIntegerTo: (anInt)
    (withMagnitude: LargeNegativeInteger 
      (+ magnitude (magnitude (asLargeInteger anInt)))))

  (method addSmallIntegerTo: (anInt)
    (+ self (asLargeInteger anInt)))

  (method * (anInt) (multiplyByLargeNegativeInteger: anInt self))

  (method multiplyByLargeNegativeInteger: (anInt)
    (withMagnitude: LargePositiveInteger
      (* magnitude (magnitude anInt))))

  (method multiplyByLargePositiveInteger: (anInt)
    (withMagnitude: LargeNegativeInteger
      (* magnitude (magnitude anInt))))

  (method multiplyBySmallInteger: (anInt)
    (* self (asLargeInteger anInt)))

  ; (method = (anInt)
  ;   (equalToLargeNegativeInteger: anInt self)

  ; (method equalToLargePositiveInteger: (anInt)
  ;   (and (= magnitude 0) (= (magnitude anInt) 0)))

  ; (method equalToLargeNegativeInteger: (anInt)
  ;   (= magnitude (magnitude anInt)))

  (method equalToSmallInteger: (anInt)
    (= self (asLargeInteger anInt)))

  (method smallIntegerLessThan: (anInteger)
    (< self (asLargeInteger anInteger)))

  (method smallIntegerGreaterThan: (anInteger)
    (> self (asLargeInteger anInteger)))

  (method div: (anInt)
    (if (isZero anInt)
      {(error: self 'division-by-zero)}
      {(if (< anInt 0)
        ; result positive
        {(withMagnitude: LargePositiveInteger
                                      (div: magnitude (magnitude anInt)))}
        ; result negative
        {(withMagnitude: LargeNegativeInteger 
                                      (div: magnitude (magnitude anInt)))})}))

  (method print () (print '-) (printNat magnitude))

  (method smallIntegerGreaterThan: (anInt) (< self (asLargeInteger anInt)))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  SmallInteger Class

(class SmallInteger SmallInteger
  ()
  (method asLargeInteger () (new: LargeInteger self))

  (method + (aNumber) (addSmallIntegerTo: aNumber self))
  (method addSmallIntegerTo: (anInteger)
    (value (addSmall:withOverflow: self anInteger
                         {(+ (asLarfgeInteger self) anInteger)})))
  (method addSmall:withOverflow: primitive add:withOverflow:)

  (method * (aNumber) (multiplyBySmallInteger: aNumber self))
  (method multiplyBySmallInteger: (anInteger)
    (value (multiplyBySmall:withOverflow: self anInteger
                         {(* (asLargeInteger self) anInteger)})))
  (method multiplyBySmall:withOverflow: primitive mul:withOverflow:)

  (method - (aNumber) (+ self (negated aNumber)))

  (method negated () (* self (new: SmallInteger -1)))

  (method addLargeNegativeIntegerTo: (anInt)
    (addLargeNegativeIntegerTo: (asLargeInteger self) anInt))

  (method addLargePositiveIntegerTo: (anInt)
    (addLargePositiveIntegerTo: (asLargeInteger self) anInt))

  (method multiplyByLargeNegativeInteger: (anInt)
    (multiplyByLargeNegativeInteger: (asLargeInteger self) anInt))

  (method multiplyByLargePositiveInteger: (anInt)
    (multiplyByLargePositiveInteger: (asLargeInteger self) anInt))

  (method = (anInt) (equalToSmallInteger: anInt self))

  (method equalToSmallInteger: primitive eqObject)

  (method isZero ()
    (equalToSmallInteger: self (new: SmallInteger 0)))
)



















