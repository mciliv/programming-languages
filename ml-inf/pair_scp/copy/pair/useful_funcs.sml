(* utility functions on type constraints 515a *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
  end

(* shared utility functions on Hindley-Milner types 485a *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end

(* representation of Hindley-Milner types 482 *)
type tyvar  = name
datatype ty = TYVAR  of tyvar               (* type variable alpha *)
            | TYCON  of tycon               (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

(* sets of free type variables in Hindley-Milner types 511a *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  reverse (f (t, emptyset))
  end

fun member x = 
  List.exists (fn y => y = x)

(* shared utility functions on Hindley-Milner types 485d *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then idsubst else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = if member a (freetyvars tau) then
                          raise BugInTypeInference "non-idempotent substitution"
                        else
                          bind (a, tau, emptyEnv)

(* utility functions on type constraints 516b *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau, tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)

(* functions that create or compare Hindley-Milner types with named type constructors 487 *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val alpha    = TYVAR "a"
val beta     = TYVAR "b"
val unittype = TYCON "unit"
fun listtype ty = 
  CONAPP (TYCON "list", [ty])
fun pairtype (x, y) =
  CONAPP (TYCON "pair", [x, y])
fun funtype (args, result) = 
  CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args), result])
fun asFuntype (CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args),
                                                                     result])) =

val tyscheme =
      usageParsers [("(forall (tyvars) type)",
                     curry FORALL <$> bracket ("('a ...)", distinctTyvars) <*>
                                                                            ty)]

(* simple implementations of set operations 982a *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs

(* TODO: Delete *)
(* TODO: Can simplify to inttype, alpha, etc. see reciation *)
(* Solve test cases *)

(****************************** True *******************************)
(*val _ = if solves (solve (TYVAR "a" ~ TYVAR "b" /\ TYVAR "b" ~ TYCON "bool"),
                 (TYVAR "a" ~ TYVAR "b" /\ TYVAR "b" ~ TYCON "bool"))
          then println "True"
          else println "False"

val _ = if solves (solve (TYCON "int" ~ TYVAR "b"),
                         (TYCON "int" ~ TYVAR "b"))
          then println "True"
          else println "False"

val _ = if solves (solve (TYCON "int" ~ TYCON "int"),
                         (TYCON "int" ~ TYCON "int"))
          then println "True"
          else println "False"

val _ = if solves (solve (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYVAR "b"),
                         (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYVAR "b"))
          then println "True"
          else println "False"

val _ = if solves (solve (CONAPP (TYCON "list", [TYVAR "a"]) ~
                          CONAPP (TYCON "list", [TYVAR "a"])),
                         (CONAPP (TYCON "list", [TYVAR "a"]) ~
                          CONAPP (TYCON "list", [TYVAR "a"])))
          then println "True"
          else println "False"*)

(****************************** Exceptions *******************************)
(*val _ = if solves (solve (CONAPP (TYCON "pair", [TYVAR "a", TYVAR "b"]) ~
                          CONAPP (TYCON "list", [TYVAR "a"])),
                         (CONAPP (TYCON "pair", [TYVAR "a", TYVAR "b"]) ~
                          CONAPP (TYCON "list", [TYVAR "a"])))
          then println "True"
          else println "False"

val _ = if solves (solve (TYCON "int" ~ CONAPP (TYCON "list", [TYVAR "a"])),
                         (TYCON "int" ~ CONAPP (TYCON "list", [TYVAR "a"])))
          then println "True"
          else println "False"

val _ = if solves (solve (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYCON "int"),
                         (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYCON "int"))
          then println "True"
          else println "False"   
            
val _ = if solves (solve (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYVAR "a"),
                         (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYVAR "a"))
          then println "True"
          else println "False"            

val _ = if solves (solve (TYCON "int" ~ TYCON "bool"),
                         (TYCON "int" ~ TYCON "bool"))
          then println "True"
          else println "False"

val _ = if solves (solve (TYVAR "a" ~ TYVAR "b" /\ TYVAR "b" ~ CONAPP (TYCON "list", [TYVAR "a"])),
                 (TYVAR "a" ~ TYVAR "b" /\ TYVAR "b" ~ CONAPP (TYCON "list", [TYVAR "a"])))
          then println "True"
          else println "False"
*)

