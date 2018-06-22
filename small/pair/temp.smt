(class LargePositiveInteger LargeInteger

  (method strictlyPositive () (not (isZero self)))
  
  (method negative () false)

  (method nonnegative () true)

  (method negated () (withMagnitude: LargeNegativeInteger magnitude))
  
  (method + (anInt) (addLargePositiveIntegerTo: anInt self))

  (method addLargePositiveIntegerTo: (anInt)
    (withMagnitude: LargePositiveInteger (+ magnitude (magnitude anInt))))
  
  (method * (anInt) (mulbyLargePositiveIntegerTo: anInt self))

  (method mulbyLargeNegativeIntegerTo: (anInt)
    (if (isZero anInt)
      ; result negative
      {(withMagnitude: LargeNegativeInteger (* magnitude (magnitude anInt)))}
      ; result positive or zero
      {(withMagnitude: LargePositiveInteger (* magnitude (magnitude anInt)))}))
  
  (method div: (anInt)
    (if (= anInt 0)
      {(error: self 'division-by-zero)}
      {(if (< anInt 0)
        ; result negative
        {(withMagnitude: LargeNegativeInteger 
                                      (div: magnitude (magnitude anInt)))}
        ; result positive
        {(withMagnitude: LargePositiveInteger 
                                      (div: magnitude (magnitude anInt))})}))) 

  (method print () )

  (method smallIntegerGreaterThan: (anInt) (> self (asLargeInteger anInt)))
)


(class LargeNegativeInteger LargeInteger
  
  (method strictlyPositive () (isZero self))

  (method negative () true)

  (method nonnegative () false)

  (method negated () (withMagnitude: LargePositiveInteger magnitude))
 
  (method + (anInt) (addLargeNegativeIntegerTo: anInt self))

  (method addLargeNegativeIntegerTo: (anInt)
    (if (< (magnitude anInt) magnitude)
      ; result negative 
      {(withMagnitude: LargeNegativeInteger (- magnitude (magnitude anInt)))}
      ; result positive or zero
      {(withMagnitude: LargePositiveInteger (- (magnitude anInt) magnitude))}))
  
  (method * (anInt) (mulbyLargeNegativeIntegerTo: anInt self))

  (method mulbyLargeNegativeIntegerTo: (anInt)
    (if (<= anInt 0)
      ; result positive or zero
      {(withMagnitude: LargePositiveInteger (* magnitude (magnitude anInt)))}
      ; result negative
      {(withMagnitude: LargeNegativeInteger (* magnitude (magnitude anInt)))}))

  (method div: (anInt)
    (if (isZero anInt)
      {(error: self 'division-by-zero)}
      {(if (< anInt 0)
        ; result positive
        {(withMagnitude: LargePositiveInteger
                                        (div: magnitude (magnitude anInt))}
        ; result negative
        {(withMagnitude: LargeNegativeInteger 
                                        (div: magnitude (magnitude anInt)))})}))

  (method print () (print '-))

  (method smallIntegerGreaterThan: (anInt) (> self (asLargeInteger anInt)))

)
