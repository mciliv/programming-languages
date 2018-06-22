(*
 * By: Morgan Ciliv
 *
 * Worked with: Azmina, Lily, Kell, Vincent Tsang, Henry Schmidt,
 * Patrick Xiao
 *)

(***** Problem A *****)
(* null: when applied to a list tells whether the list is empty *)
fun null [] = true
  | null (x :: xs) = false

val () =
        Unit.checkAssert "[] is true"
        (fn () => null [])

val () =
        Unit.checkAssert "[1] is false"
        (fn () => not (null [1]))

(***** Problem B *****)
(* firstVowel takes a list of lower-case letters and returns true if the first
   character is a vowel (aeiou) and false if the first character is not a
   vowel or if the list is empty *)

fun firstVowel [] = false
  | firstVowel (#"a" :: xs) = true
  | firstVowel (#"e" :: xs) = true
  | firstVowel (#"i" :: xs) = true
  | firstVowel (#"o" :: xs) = true
  | firstVowel (#"u" :: xs) = true
  | firstVowel ( _ :: xs) = false

val () =
        Unit.checkAssert "[] is false"
        (fn () => not (firstVowel []))

val () =
        Unit.checkAssert "[a] is true"
        (fn () => firstVowel [#"a"])

val () =
        Unit.checkAssert "[e] is true"
        (fn () => firstVowel [#"e"])

val () =
        Unit.checkAssert "[i] is true"
        (fn () => firstVowel [#"i"])

val () =
        Unit.checkAssert "[o] is true"
        (fn () => firstVowel [#"o"])

val () =
        Unit.checkAssert "[u] is true"
        (fn () => firstVowel [#"u"])

val () =
        Unit.checkAssert "[b] is false"
        (fn () => not (firstVowel [#"b"]))

(***** Problem C *****)

(* rev reverses the list *)
fun rev xs = foldl (op::) [] xs

val () =
        Unit.checkExpectWith (Unit.listString Int.toString) "[] is []"
        (fn () => rev [])
        []

val () =
        Unit.checkExpectWith (Unit.listString Int.toString) "[1] is [1]"
        (fn () => rev [1])
        [1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString) "[1, 2] is [2, 1]"
        (fn () => rev [1, 2])
        [2, 1]

(* minlist returns the smallest element of a non-empty list of integers. If
   the list is empty, the exception Match is raised. *)
fun minlist [] = raise Match
  | minlist (x :: xs) = foldl Int.min x xs


val () =
        Unit.checkExnWith (Int.toString) "[] is an exception"
        (fn () => minlist [])

val () =
        Unit.checkExpectWith (Int.toString) "[1] is 1"
        (fn () => minlist [1])
        1

val () =
        Unit.checkExpectWith (Int.toString) "[1, 2] is 1"
        (fn () => minlist [1, 2])
        1

(***** Problem D *****)

exception Mismatch

(* zip takes a pair of lists (of equal length) and returns the equivalent list
   of pairs. If the lengths don't match the exception Mismatch is raised. *)

fun zip ([], []) = []
  | zip ((x :: xs), []) = raise Mismatch
  | zip ([], (y :: ys)) = raise Mismatch
  | zip ((x :: xs), (y :: ys)) = (x, y) :: zip (xs, ys)

val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[] [] is []"
        (fn () => zip ([], []))
        []

val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[x] [y] is [(x,y)]"
        (fn () => zip ([1], [2]))
        [(1, 2)]

val () =
        Unit.checkExnWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[1] [] is an error"
        (fn () => zip ([1], []))

val () =
        Unit.checkExnWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[] [2] is an error"
        (fn () => zip ([], [2]))

(* Don't need this test. *)
val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[1, 3] [2, 4] is [(1, 2), (3, 4)]"
        (fn () => zip ([1, 3], [2, 4]))
        [(1, 2), (3, 4)]

(***** Problem E *****)

(* pairfoldr applies a three-argument function to a pair of lists of equal length, using the same order as foldr. *)

fun pairfoldr f b ([], []) = b
  | pairfoldr f b ((x::xs), (y::ys)) = f (x, y, (pairfoldr f b (xs, ys)))
  | pairfoldr _ _ (_, _) = raise Mismatch

(* zip2 is like zip but uses pairfoldr for its implementation *)

fun zip2 ((x :: xs), []) = raise Mismatch
  | zip2 ([], (y :: ys)) = raise Mismatch
  | zip2 (xs, ys) = pairfoldr (fn (x, y, list) => (x, y)::list) [] (xs, ys)

val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[] [] is []"
        (fn () => zip2 ([], []))
        []

val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[x] [y] is [(x,y)]"
        (fn () => zip2 ([1], [2]))
        [(1, 2)]

val () =
        Unit.checkExnWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[1] [] is an error"
        (fn () => zip2 ([1], []))

val () =
        Unit.checkExnWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[] [2] is an error"
        (fn () => zip2 ([], [2]))

(* Don't need this test. *)
val () =
        Unit.checkExpectWith
          (Unit.listString (Unit.pairString Int.toString Int.toString))
          "[1, 3] [2, 4] is [(1, 2), (3, 4)]"
        (fn () => zip2 ([1, 3], [2, 4]))
        [(1, 2), (3, 4)]

(***** Problem F *****)

(* concat takes a list of lists and produces a single list containing all the elements in the correct order. *)

fun concat [] = []
  | concat ([]::xs) = concat xs
  | concat ((x::xs)::ys) = x::(concat (xs::ys))


val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "[] is []"
        (fn () => concat [])
        []

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "[[]] is []"
        (fn () => concat [[]])
        []

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "[[[1], [2, 3, 4], [], [5, 6]] is [1, 2, 3, 4, 5, 6]"
        (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
        [1, 2, 3, 4, 5, 6]

(***** Problem G *****)

type nat = int list

(* intOfNat converts a list of digits into a machine integer, or if the number
represented by the lists of digits is too large, raises Overflow. *)
fun intOfNat [] = 0
  | intOfNat (x::[]) = x
  | intOfNat (x::xs) = (intOfNat xs) * 10 + x

val () =
        Unit.checkExpectWith (Int.toString)
        "[] is 0"
        (fn () => intOfNat [])
        0

val () =
        Unit.checkExpectWith (Int.toString)
        "[1] is 1"
        (fn () => intOfNat [1])
        1

val () =
        Unit.checkExpectWith (Int.toString)
        "[3, 2, 1] is 321"
        (fn () => intOfNat [3, 2, 1])
        123

(* natOfInt converts a nonnegative machine integer into a natural number. *)
fun natOfInt 0 = []
  | natOfInt n = if n < 10
                      then [n]
                      else (n mod 10)::(natOfInt (n div 10))

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "0 is []"
        (fn () => natOfInt 0)
        []

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "1 is [1]"
        (fn () => natOfInt 1)
        [1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "12 is [2, 1]"
        (fn () => natOfInt 12)
        [2, 1]

val natString = String.concat o map Int.toString o rev

val () =
        Unit.checkExpectWith String.toString
        "[3, 2, 1] -> 123"
        (fn () => natString [3, 2, 1])
        "123"

(***** Problem H *****)

(* (carryIntoNat takes a natural number n and a carry bit c, and it returns n
   + c. A carry bit is either 0 or 1. *)
fun carryIntoNat (n, 0) = n
  | carryIntoNat ([], c) = natOfInt c
  | carryIntoNat (d::m, _) = ((d+1) mod 10)::(carryIntoNat (m, (d+1) div 10))

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([], 1) -> [1]"
        (fn () => carryIntoNat ([], 1))
        [1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([9, 1], 0) -> [9, 1]"
        (fn () => carryIntoNat ([9, 1], 0))
        [9, 1]

val () =
      Unit.checkExpectWith (Unit.listString Int.toString)
          "([9, 1], 1) -> [0, 2]"
        (fn () => carryIntoNat ([9, 1], 1))
        [0, 2]

(* addWithCarry takes two natural numbers n1 and n2, and a carry bit c, and 
  returns n1 + n2 + c *)

fun addWithCarry (n1, [], c) = carryIntoNat (n1, c)
  | addWithCarry ([], n2, c) = carryIntoNat (n2, c)
  | addWithCarry (d1 :: m1, d2 :: m2, c) =
      let val d  = (d1 + d2 + c) mod 10
          val c' = (d1 + d2 + c) div 10 (* the "carry out" *)
      in (d :: addWithCarry (m1, m2, c'))
      end

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([9], [], 1) is [0, 1]"
        (fn () => addWithCarry ([9], [], 1))
        [0, 1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([], [9, 9], 0) is [9, 9]"
        (fn () => addWithCarry ([], [9, 9], 0))
        [9, 9]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([9, 9], [9, 9], 1) is [9, 9, 1]"
        (fn () => addWithCarry ([9, 9], [9, 9], 1))
        [9, 9, 1]

(* addNats adds two natural numbers *)
fun addNats (n1, n2) = addWithCarry (n1, n2, 0)

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([9, 9], [9, 9]) is [8, 9, 1]"
        (fn () => addNats ([9, 9], [9, 9]))
        [8, 9, 1]

(* borrowFromNat takes a natural number n and a borrow bit b, and it returns n
 * - b, provided that n - b is a natural number. If n - b is not a natural
 * number, borrowFromNat raises the exception Negative. *)

exception Negative

fun borrowFromNat (n, 0) = n
  | borrowFromNat (0::m, 1) = 9::(borrowFromNat (m, 1))
  | borrowFromNat ([1], 1) = []
  | borrowFromNat (d::m, 1) = (d-1)::m
  | borrowFromNat (_, _) = raise Negative
  (* TODO: Is this format okay? *)

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([1], 0) is [1]"
        (fn () => borrowFromNat ([1], 0))
        [1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([0, 1], 1) is [9]"
        (fn () => borrowFromNat ([0, 1], 1))
        [9]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([0, 2], 1) is [9, 1]"
        (fn () => borrowFromNat ([0, 2], 1))
        [9, 1]

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([1, 1], 1) is [0, 1]"
        (fn () => borrowFromNat ([1, 1], 1))
        [0, 1]

val () =
        Unit.checkExnWith (Unit.listString Int.toString)
        "([], 1) is a Negative exception"
        (fn () => borrowFromNat ([], 1))

(* subWithBorrow takes two natural numbers n1 and n2, and a borrow bit b, and
 * if n1 - n2 - b is a natural number, it returns n1 - n2 - b. Otherwise it
 * raises the Negative exception *)

fun subWithBorrow (n1, [], b) = borrowFromNat (n1, b)
  | subWithBorrow (d1::m1, d2::m2, b) =
      let val d = (d1 - d2 - b) mod 10
          val b' = if d1 - d2 - b < 0 then 1 else 0 (* the "borrow out" *)
      in d::subWithBorrow (m1, m2, b')
      end
  | subWithBorrow (_, _, _) = raise Negative

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([1], [], 1)"
        (fn () => subWithBorrow ([1], [], 1))
        []

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([2], [1], 1)"
        (fn () => subWithBorrow ([2], [1], 1))
        [0] (* Represents the same thing as [] *)

val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "([1, 1], [2], 0)"
        (fn () => subWithBorrow ([1, 1], [2], 0))
        [9]

(* subNats subtracts two natural numbers *)
fun subNats (n1, n2) = subWithBorrow (n1, n2, 0)

val () =
        Unit.checkExnSatisfiesWith natString "1 - 5"
        (fn () => subNats ([1], [5]))
        ("Negative", fn Negative => true | _ => false)


val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)