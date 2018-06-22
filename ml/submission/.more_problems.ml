(***** Problem J *****)

datatype 'a tree = NODE of 'a tree * 'a * 'a tree 
                 | LEAF
datatype 'a set = SET of ('a * 'a -> order) * 'a tree
fun nullset cmp = SET (cmp, LEAF)

fun addelt (elem, set) = set

val addelt : 'a * 'a set -> 'a set = addelt
(*fun treeFoldr : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b = treeFoldr
val setFold : ('a * 'b -> 'b) -> 'b -> 'a set -> 'b = setFold
val singletonOf : 'a -> 'a flist = singletonOf
val atFinger : 'a flist -> 'a = atFinger
val fingerLeft  : 'a flist -> 'a flist = fingerLeft
val fingerRight : 'a flist -> 'a flist = fingerRight
val deleteLeft  : 'a flist -> 'a flist = deleteLeft
val deleteRight : 'a flist -> 'a flist = deleteRight
val insertLeft  : 'a * 'a flist -> 'a flist = insertLeft
val insertRight : 'a * 'a flist -> 'a flist = insertRight
val ffoldl : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b = ffoldl
val ffoldr : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b = ffoldr*)