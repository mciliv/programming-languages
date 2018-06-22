(***** Problem I *****)

exception NotFound of string

fun isBound (n, env) =
  let val answer = lookup (n, env)
  in true
    handle NotFound (n) => false
  end

val _ = op isBound : string * 'a env -> bool