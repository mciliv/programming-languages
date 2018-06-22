(***** Problem I *****)

type 'a env = (string * 'a) list
exception NotFound of string
val emptyEnv : 'a env = []
val bindVar : string * 'a * 'a env -> 'a env =
  (fn (name, value, env) => (name, value)::env)
fun lookup (name, (n, v)::rest) = if name = n then v else lookup (name, rest)
  | lookup (name, []) = raise NotFound (name)

val _ = op lookup: string * 'a env -> 'a

(* How do I set return value correctly with these Unit tests? *)
(*val () =
        Unit.checkExpectWith (Unit.showNothing) 
        "('a', [('a', 1)]) is 1"
        (fn () => lookup ("a", [("a", 1)]))
        1

val () =
        Unit.checkExnWith (Unit.showNothing)
        "('a', [])"
        (fn () => lookup ("a", []))*)

(* val () = Unit.reportWhenFailures () *) (* put me at the _end_ *)
