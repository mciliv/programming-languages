(* Succeeds *)
val _ = constraintTest (TYVAR "a" ~ TYCON "int" /\
          TYVAR "b" ~ CONAPP (TYCON "list", [TYVAR "a"]))

(* Fails *)
val _ = constraintTest (TYVAR "a" ~ TYVAR "b" /\
          TYVAR "a" ~ CONAPP (TYCON "list", [TYVAR "b"]))

(* Fails *)
val _ = constraintTest (TYVAR "a" ~ TYCON "int" /\ TYVAR "a" ~ TYCON "bool")