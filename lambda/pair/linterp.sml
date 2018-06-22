(***************** Problem 1: Basics of evaluation *****************)

  (* part 1: representation of terms *)

datatype void = VOID of void  (* a type with no values *)

datatype term = VAR of string
              | LAM of string * term
              | APP of term * term

exception LeftAsExercise of string

val var : string -> term         = fn x => VAR x
val app : term   -> term -> term = fn e => fn e' => APP (e, e')
val lam : string -> term -> term = fn x => fn e => LAM (x, e)

val cpsLambda :
    term ->
    (string -> term -> 'a) ->
    (term -> term -> 'a) ->
    (string -> 'a) ->
    'a
  = fn LAM (x, e)   => (fn f => fn g => fn h => f x e)
     | APP (e, e')  => (fn f => fn g => fn h => g e e')
     | VAR x        => (fn f => fn g => fn h => h x)

(* part 2: conversion to uScheme syntax *)
fun toString t =
  let fun vartoString x    = x
      fun lamtoString x e  = "(lambda (" ^ x ^ ") " ^ (toString e) ^ ")"
      fun apptoString e e' = "(" ^ (toString e) ^ " " ^ (toString e') ^ ")"
  in cpsLambda t lamtoString apptoString vartoString
  end

val _ = op toString : term -> string



(***************** Problem 2: substitution *****************)
(* set operation funcitons copied from previous homework provided by Norman *)
type 'a set = 'a list
val emptyset = []
fun member x =
  List.exists (fn y => y = x)
fun insert (x, ys) =
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs

fun freeIn y exp =
  let fun varFreeIn x = x = y
      fun lamFreeIn x e = not (x = y) andalso freeIn y e
      fun appFreeIn M N = freeIn y M orelse freeIn y N
  in cpsLambda exp lamFreeIn appFreeIn varFreeIn
  end
(* we suggest a useful auxiliary function, freeVars *)
fun freeVars t =
  let fun varFreeVars x = [x]
      fun lamFreeVars x e = List.filter (fn a => not (a = x)) (freeVars e)
      fun appFreeVars M N = union (freeVars M, freeVars N)
  in cpsLambda t lamFreeVars appFreeVars varFreeVars
  end

fun freshVar avoid =
    let val n = ref 0
        fun f n =
          let val x = ("x" ^ Int.toString (!n) before n:= !n + 1)
          in if List.exists (fn a => (a = x)) avoid then f n
             else x
          end
    in f n
    end

fun subst (x, N) t =
  let fun varSubst y = if x = y then N else var y (* law a + b *)
      fun appSubst M1 M2 = app (subst (x, N) M1) (subst (x, N) M2) (*law c*)
      fun lamSubst y M =
           if x = y then lam x M   (*law d*)
           else
                if (not (freeIn x M) orelse not (freeIn y N))
                then lam y (subst (x, N) M)  (*law e*)
                else
                    let val newvar = freshVar (union (freeVars M, freeVars N))
                    in subst (x, N) (lam newvar (subst (y, var newvar) M))
                    end
  in cpsLambda t lamSubst appSubst varSubst
  end

val _ = op freeIn   : string -> term -> bool
val _ = op freeVars : term  -> string list
val _ = op freshVar : string list -> string
val _ = op subst    : string * term -> term -> term

(***************** Problem 3: reductions *****************)

  (* we pretend *every* term is in normal form --- obviously wrong,
     but a useful way to get started *)

exception NormalForm

fun reduceN (APP ((LAM (x, M)), N)) = subst (x, N) M
  | reduceN (LAM (x, (APP (M , VAR y)))) =
        if (x = y andalso not (freeIn x M)) then M
        else lam x (reduceN (app M (var y)))
  | reduceN (APP (M, N)) =
        (app (reduceN M) N handle NormalForm => app M (reduceN N))
  | reduceN (LAM (x, M)) = lam x (reduceN M)
  | reduceN (VAR x) = raise NormalForm

fun reduceA (APP ((LAM (x, M)), N)) =
        (reduceA N handle NormalForm => subst (x, N) M)
  | reduceA (LAM (x, (APP (M , VAR y)))) =
        if (x = y andalso not (freeIn x M)) then M
        else lam x (reduceN (app M (var y)))
  | reduceA (APP (M, N)) =
        (app (reduceN N) M handle NormalForm => app N (reduceN M))
  | reduceA (LAM (x, M)) = lam x (reduceA M)
  | reduceA (VAR x) = raise NormalForm

val _ = op reduceN : term -> term
val _ = op reduceA : term -> term
(* for testing, you may want to change these variables *)

val reducer       = reduceN
val defaultMax = 100000
val maxReductions =
  case OS.Process.getEnv "MAXRED"
    of NONE => defaultMax
     | SOME s => getOpt (Int.fromString s, defaultMax)


(**************************************************)
(*                                                *)
(*       TO COMPLETE THE LAMBDA ASSIGNMENT,       *)
(*         YOU SHOULD NOT NEED TO MODIFY          *)
(*             ANYTHING PAST THIS LINE            *)
(*                                                *)
(**************************************************)

val normalize : term -> term = Lhelp.normalize cpsLambda reducer maxReductions


(**************************************************)
(*                                                *)
(*   PARSING                                      *)
(*                                                *)
(**************************************************)
type name = string
datatype def = VAL    of {reduce:bool} * name * term
             | TERM   of term
             | USE    of name
             | CHECK_EQUIV of term * term

datatype Token = TokLambda
               | TokDot
               | TokUse of string
               | TokEq
               | TokNoreduce (* the keyword 'noreduce' *)
               | TokCheckEquiv (* the keyword 'check-equiv' *)
               | TokVar of string
               | TokOpenParen
               | TokCloseParen
               | TokEOT
               | TokEOF

fun split pred =
    let fun helper acc (x::xs) =
            if pred x then (rev acc, x::xs) else helper (x::acc) xs
          | helper acc [] = (rev acc, [])
    in helper [] end

fun tokenize nextline firstline =
  let fun isFilenameChar #";" = false
        | isFilenameChar c = Char.isGraph c
      fun isVarChar c = not (Char.contains "\\./();=" c) andalso Char.isGraph c
      fun getfilename s =
            let val (x, rest)  = split (not o Char.isSpace)   s
                val (n, rest') = split (not o isFilenameChar) rest
            in  (String.implode n, rest')
            end
      fun helper (acc as (TokEOT::_)) [] = rev acc
        | helper acc [] = helper acc (String.explode (nextline()))
        | helper acc (#"\\"::rest) = helper (TokLambda::acc) rest
        | helper acc (#"."::rest)  = helper (TokDot::acc) rest
        | helper acc (#"/"::(#"/"::rest))  = helper acc []
        | helper acc (#"("::rest)  = helper (TokOpenParen::acc) rest
        | helper acc (#")"::rest)  = helper (TokCloseParen::acc) rest
        | helper acc (#";"::rest)  = helper (TokEOT::acc) rest
        | helper acc (#"="::rest)  = helper (TokEq::acc) rest
        | helper acc (l as (c::rest)) =
          let val (name, rest') = split (not o isVarChar) l
          in case String.implode name
              of ""    => helper acc rest
               | "use" => let val (fname,rest'') = getfilename rest'
                          in  helper (TokUse fname::acc) rest''
                          end
               | "noreduce" => helper (TokNoreduce::acc) rest'
               | "check-equiv" => helper (TokCheckEquiv::acc) rest'
               | name  => helper (TokVar name::acc) rest'
          end
  in  helper [] (String.explode firstline)
  end

exception SyntaxError of string

fun parseTerm toks =
    let fun appchain NONE term = term
          | appchain (SOME pterm) term = app pterm term
        fun helper prev (TokLambda::(TokVar var)::TokDot::rest) =
            let val (body, rest') = helper NONE rest
            in (appchain prev (lam var body), rest') end
          | helper prev (TokOpenParen::rest) =
            let val (term, rest') = helper NONE rest
            in helper (SOME (appchain prev term)) rest' end
          | helper (SOME prev) (TokCloseParen::rest) = (prev, rest)
          | helper prev ((TokVar name)::rest) =
            helper (SOME (appchain prev (var name))) rest
          | helper (SOME prev) (rest as (TokEOF::_)) = (prev, rest)
          | helper (SOME prev) (TokEOT::rest) = (prev, rest)
          | helper _ (TokNoreduce::_) =
               raise (SyntaxError "'noreduce' is a reserved word")
          | helper _ _ = raise (SyntaxError "Invalid Lambda Expression")
    in helper NONE toks end

fun takewhile p [] = []
  | takewhile p (x::xs) = if p x then x :: takewhile p xs else []
fun dropwhile p [] = []
  | dropwhile p (x::xs) = if p x then dropwhile p xs else x :: xs

fun parseDef (TokUse filename::TokEOT::rest) = USE filename :: parseDef rest
  | parseDef (TokVar name::TokEq::rest) =
    let val (trm, rest') = parseTerm rest
    in VAL ({reduce=true}, name, trm) :: parseDef rest' end
  | parseDef (TokNoreduce::TokVar name::TokEq::rest) =
    let val (trm, rest') = parseTerm rest
    in VAL ({reduce=false},name, trm) :: parseDef rest' end
  | parseDef (TokCheckEquiv::rest) =
    let fun iseq (TokEq) = true
          | iseq _       = false
    in  case parseTerm (takewhile (not o iseq) rest @ [TokEOT])
          of (t1, []) =>
               (case dropwhile (not o iseq) rest
                  of TokEq :: rest =>
                       let val (t2, rest) = parseTerm rest
                       in  CHECK_EQUIV (t1, t2) :: parseDef rest
                       end
                     | _ =>
                     raise SyntaxError
                            "Expected = sign after first term in 'check-equiv'")
           | _ => raise SyntaxError
                    "Invalid Lambda Expression or missing = after 'check-equiv'"
    end
  | parseDef [] = []
  | parseDef toks =
    let val (trm, rest) = parseTerm toks
    in TERM trm :: parseDef rest end

(**************************************************)
(*                                                *)
(*   CONVERTING TERMS TO STRINGS                  *)
(*                                                *)
(**************************************************)

val unparse = Lhelp.toString cpsLambda

(**************************************************)
(*                                                *)
(*   ENVIRONMENTS                                 *)
(*                                                *)
(**************************************************)

(* environments 186 *)
type name = string
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and assignment of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find(name, tail)

fun isBound env name = (find (name, env); true) handle NotFound _ => false

(* adding new bindings *)
exception BindListLength
fun bind(name, v, rho) = (name, v) :: rho
fun bindList(n::vars, v::vals, rho) = bindList(vars, vals, bind(n, v, rho))
  | bindList([], [], rho) = rho
  | bindList _ = raise BindListLength
(* type declararations for consistency checking *)
val _ = op emptyEnv : 'a env
val _ = op find     : name * 'a env -> 'a
val _ = op bind     : name      * 'a      * 'a env -> 'a env
val _ = op bindList : name list * 'a list * 'a env -> 'a env


(**************************************************)
(*                                                *)
(*   READERS                                      *)
(*                                                *)
(**************************************************)

exception EOF

type reader = unit -> string (* raises EOF *)

fun filereader fd () =
  case TextIO.inputLine fd of SOME line => line | NONE => raise EOF

fun stringsreader l =
  let val buffer = ref l
  in  fn () => case !buffer
                 of [] => raise EOF
                  | h :: t => h before buffer := t
  end

val _ = op filereader    : TextIO.instream -> reader
val _ = op stringsreader : string list     -> reader

type defreader = { buffer    : def list ref
                 , nextline  : unit -> string
                 , firstline : unit -> string
                 }
fun readdef (r as { buffer, nextline, firstline }) =
  case !buffer
    of h::t => h before buffer := t
     | []   => ( buffer := parseDef (tokenize nextline (firstline()))
               ; readdef r
               )

fun defreader (getline, prompt) =
  let fun promptIn prompt () = ( TextIO.output(TextIO.stdOut, prompt)
                               ; TextIO.flushOut(TextIO.stdOut)
                               ; getline ()
                               )
  in  if prompt then
        { buffer = ref [], nextline = promptIn "   ", firstline = promptIn "-> "
                                                                               }
      else
        { buffer = ref [], nextline = getline, firstline = getline }
  end

fun echoTag f x =
  let val line = f x
      val _ = if (String.substring (line, 0, 2) = ";#" handle _ => false) then
                print line
              else
                ()
  in  line
  end

fun echoBuf { buffer=b, nextline=n, firstline=f } =
 { buffer=b, nextline=echoTag n, firstline=echoTag f }

val defreader = fn args => echoBuf (defreader args)

(**************************************************)
(*                                                *)
(*   IMPLEMENTATION OF [[USE]]                    *)
(*                                                *)
(**************************************************)

fun printerr s = TextIO.output (TextIO.stdErr, s)

(* implementation of [[use]] 193c *)
fun use readEvalPrint filename rho =
      let val fd = TextIO.openIn filename
          fun writeln s = List.app print    [s, "\n"]
          fun errln s   = List.app printerr [s, "\n"]
      in  readEvalPrint (defreader (filereader fd, false), writeln, errln) rho
          before TextIO.closeIn fd
      end

(**************************************************)
(*                                                *)
(*   TERM EQUIVALENCE                             *)
(*                                                *)
(**************************************************)

local
  fun dropWhile p [] = []
    | dropWhile p (x::xs) = if p x then dropWhile p xs else x :: xs

  fun varlist 0 t = "0" :: t
    | varlist n t = varlist (n-1) (Int.toString n :: t)

  val vars = varlist 100 []

  fun freeIn x t = List.exists (fn y => y = x) (freeVars t)

  fun alphaNormalize t =
    let fun n vars t =
          let fun ofLam x t =
                let fun noGood x' = x <> x' andalso freeIn x' t
                in  case dropWhile noGood vars
                      of x' :: xs => lam x' (n vars (subst (x, var x') t))
                       | [] => let exception OutOfVars in raise OutOfVars end
                end
              fun ofApp t1 t2 = app (n vars t1) (n vars t2)
              fun ofVar x = var x
          in  cpsLambda t ofLam ofApp ofVar
          end
    in  n vars t
    end
in
  fun termeq (t1, t2) = (alphaNormalize t1 = alphaNormalize t2)

  val testingSuppressed =
    (subst ("x", var "x") (var "x"); false) handle LeftAsExercise _ => true
end


(**************************************************)
(*                                                *)
(*   EVALUATION                                   *)
(*                                                *)
(**************************************************)

exception FreeVarInLet of name

fun eval t rho set reduce =
    let fun expand (v, t) =
          if isBound rho v then app (lam v t) (find (v, rho))
          else if not set  then t
          else                  raise (FreeVarInLet v)
        val t = foldr expand t (freeVars t)
    in  if reduce then normalize t else t
    end

val _ = op eval : term -> term env -> bool -> bool -> term

fun tag s e = (print s; print "\n"; e)

fun evaldef (t, rho, echo) =
  case t
    of USE filename  => use readEvalPrint filename rho
     | VAL ({reduce}, name, t) =>
         let val result = eval t rho true reduce
             handle Lhelp.Diverged =>
                 ( List.app print ["DIVERGENT DEFINITION ", name, "\n"]
                 ; eval t rho true false
                 )
         in  bind (name, result, rho) before echo name
         end
     | TERM e =>
         let val result = eval e rho false true
             handle Lhelp.Diverged =>
                 ( List.app print ["DIVERGENT TERM ", unparse e, "\n"]
                 ; e
                 )
         in  bind ("it", result, rho) before echo (unparse result)
         end
     | CHECK_EQUIV (e1, e2) =>
         rho before
         (if testingSuppressed then ()
          else
            let val r1 =
                  SOME (eval e1 rho false true) handle Lhelp.Diverged => NONE
                val r2 =
                  SOME (eval e2 rho false true) handle Lhelp.Diverged => NONE
            in  case (r1, r2)
                  of (NONE, _) =>
                       List.app print
                          ["The test failed because reduction of ", unparse e1,
                                       " did not terminate\n"]
                   | (SOME _, NONE) =>
                       List.app print
                          ["The test failed because reduction of ", unparse e2,
                                       " did not terminate\n"]
                   | (SOME r1, SOME r2) =>
                       if termeq (r1, r2) then
                         print "The test passed\n"
                       else
                         List.app print
                         ["The test failed: terms ", unparse e1, " and ",
                          unparse e2, " do not have equivalent normal forms\n"]
            end)

and readEvalPrint (reader, echo, errmsg) rho =
  let fun loop rho =
        let fun continue msg = (errmsg msg; loop rho)
            fun finish () = rho
        in  loop (evaldef (readdef reader, rho, echo))
            handle
                EOF => finish()
              (* more read-eval-print handlers 194b *)
              | IO.Io {name, ...} => continue ("I/O error: " ^ name)
              | SyntaxError msg   => continue ("syntax error: " ^ msg)
              | FreeVarInLet name =>
                  continue ("error: unbound free variable in RHS of binding: "
                            ^ name)
              | NotFound n        => continue ("variable " ^ n ^ " not found")
        end
  in  loop rho end
val _ = op evaldef : def * term env * (string->unit) -> term env
val _ = op readEvalPrint : defreader * (string->unit) * (string->unit) ->
                              term env -> term env

fun runInterpreter noisy =
  let fun writeln s = List.app print    [s, "\n"]
      fun errln   s = List.app printerr [s, "\n"]
      val rc = ref OS.Process.success
      fun asFailure x = (rc := OS.Process.failure; x)
      val _ = ignore (readEvalPrint (defreader (filereader TextIO.stdIn, noisy),
                                     writeln, asFailure o errln) emptyEnv)
              handle EOF => ()
  in  !rc
  end
val _ = op runInterpreter : bool -> OS.Process.status

(**************************************************)
(*                                                *)
(*   COMMAND LINE                                 *)
(*                                                *)
(**************************************************)
val N : term = app (app (var "fst") (var "x")) (var "y")
(* example for Norman's slides for renaming *)
val lambYN :  term = lam "no" (var "yes")
val lambNT : term = lam "time" (var "no")

val checkExpectTerm = Unit.checkExpectWith toString

val () = checkExpectTerm "subst, case (a)"
    (fn () => subst ("x", N) (var "x"))
    N

val () = checkExpectTerm "subst, case (b)"
    (fn () => subst ("x", N) (var "y"))
    (var "y")

val () = checkExpectTerm "subst, case (c)"
    (fn () => subst ("x", N) (app (var "x") (var "y")))
    (app N (var "y"))

val () = checkExpectTerm "subst, case (d)"
    (fn () => subst ("x", N) (lam "x" (var "y")))
    (lam "x" (var "y"))

val () = checkExpectTerm "subst, case (e)"
    (fn () => subst ("x", N) (lam "z" (app (var "x") (var "z"))))
    (lam "z" (app N (var "z")))

val () = checkExpectTerm "subst, renaming"
    (fn () => subst ("yes", lambNT) lambYN)
    (lam "x0" (lam "time" (var "no")))
val _ = Unit.reportWhenFailures ()

(* command line *)
fun main ["-q"] = runInterpreter false
  | main []     = runInterpreter true
  | main _      =
      (TextIO.output (TextIO.stdErr, "Usage: " ^ CommandLine.name() ^ " [-q]\n")
      ; OS.Process.failure
      )
val _ = OS.Process.exit (main (CommandLine.arguments()))