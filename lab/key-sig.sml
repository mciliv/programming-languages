signature KEY = sig
  type int      (* a key value *)

  val ordering : int * int -> order

  (* contracts:
       empty  is the empty map
       find (k, d) returns the x that d maps k to, or if d does not map k,
                   it raises `NotFound k`
       bind (k, x, d) = d' such that
                             - d' maps k to x
                             - if k' <> k, d' maps k' the same way d does
     laws:
       find (k, bind (k,  x, d)) = x
       find (k, bind (k', x, d)) = find (x, d)  if k <> k'
       find (k, empty)   raises NotFound k
   *)
end
