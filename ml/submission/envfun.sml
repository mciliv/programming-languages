(***** Problem I *****)

type 'a env = string -> 'a
exception NotFound of string
val emptyEnv : 'a env = fn n => raise NotFound n

fun bindVar (n, v, rho) = fn name => if name = n then v else rho name

val _ = op bindVar : string * 'a * 'a env -> 'a env

fun lookup (name, rho) = rho name

val _ = op lookup: string * 'a env -> 'a