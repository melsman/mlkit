(* Parts of the Edinburgh Library used by the Kit *)

structure Edlib =
 struct
   structure List: sig
       val stringSep: string -> string -> string ->
                      ('a -> string) -> 'a list -> string

       val member: ''a -> ''a list -> bool

       val removeNth: int -> 'a list -> ('a * 'a list)

       exception First of string

       val first: ('a -> bool) -> 'a list -> 'a
       val dropFirst: ('a -> bool) -> 'a list -> 'a list
       val removeFirst: ('a -> bool) -> 'a list -> ('a * 'a list)
       val splitFirst: ('a -> bool) -> 'a list -> ('a list * 'a list)

       exception Empty of string
       val removeLast: 'a list -> ('a * 'a list)

       val forAll: ('a -> bool) -> 'a list -> bool
       val dropAll: ('a -> bool) -> 'a list -> 'a list

       val foldR: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
       val foldL: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
       val foldR': ('a -> 'a -> 'a) -> 'a list -> 'a
       val foldL': ('a -> 'a -> 'a) -> 'a list -> 'a
   end = List

 end
