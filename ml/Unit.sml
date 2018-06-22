val ran = ref 0
val passed = ref 0

fun passTest () = (ran := !ran + 1; passed := !passed + 1)
fun failTest msgs = (ran := !ran + 1; app print msgs; print "\n")

fun checkExpectWith show name thunk a =
  let val a' = thunk ()
  in  if a' = a then
        passTest ()
      else
        failTest ["In test '", name, "', expected value ", show a, " but got ", show a']
  end handle e =>
    failTest ["In test '", name, "', expected value ", show a, " but got an exception"]

fun checkAssert name thunk =
  let val a' = thunk ()
  in  if a' then
        passTest ()
      else
        failTest ["In test '", name, "', the assertion was not satisfied"]
  end handle e =>
    failTest ["In test '", name, "', the assertion raised an exception"]

fun checkExnSatisfiesWith show name thunk (ename, pred) =
  let val a' = thunk ()
  in  failTest ["In test '", name, "', expected exception ", ename,
                " but got value ", show a']
  end handle e =>
    if pred e then
      passTest ()
    else
      failTest ["In test '", name, "', expected exception ", ename,
                " but got some other exception"]

fun checkExnWith show name thunk =
  checkExnSatisfiesWith show name thunk ("any exception", fn _ => true)

fun println s = (print s; print "\n")

fun reportTestResultsOf what (npassed, nthings) =
  case (npassed, nthings)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => println ("The only " ^ what ^ " failed.")
     | (1, 1) => println ("The only " ^ what ^ " passed.")
     | (0, 2) => println ("Both " ^ what ^ "s failed.")
     | (1, 2) => println ("One of two " ^ what ^ "s passed.")
     | (2, 2) => println ("Both " ^ what ^ "s passed.")
     | _ => if npassed = nthings then
               app print ["All ", Int.toString nthings, " " ^ what ^ "s passed.\n"]
            else if npassed = 0 then
               app print ["All ", Int.toString nthings, " " ^ what ^ "s failed.\n"]
            else
               app print [Int.toString npassed, " of ", Int.toString nthings,
                          " " ^ what ^ "s passed.\n"]

fun report () = reportTestResultsOf "internal Unit test" (!passed, !ran)

fun reportWhenFailures () =
  if !passed <> !ran then
    reportTestResultsOf "internal Unit test" (!passed, !ran)
  else
    ()



fun listString _ [] = "[]"
  | listString show (x::xs) =
      concat ("[" :: show x :: foldr (fn (y, tail) => ", " :: show y :: tail)
        ["]"] xs)

fun pairString showa showb (a, b) = "(" ^ showa a ^ ", " ^ showb b ^ ")"

fun showNothing _ = "a value"
