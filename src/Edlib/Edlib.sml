(* Parts of the Edinburgh Library used by the Kit *)

structure Edlib =
 struct
   structure List: sig
       val removeNth: int -> 'a list -> ('a * 'a list) (* in Compiler/Lambda/OptLambda.sml *)

       exception First of string

       val first: ('a -> bool) -> 'a list -> 'a
       val splitFirst: ('a -> bool) -> 'a list -> ('a list * 'a list)

       exception Empty of string
       val removeLast: 'a list -> ('a * 'a list) (* in Common/PrettyPrint.sml *)
       val dropAll: ('a -> bool) -> 'a list -> 'a list 
          (* in Common/EfficientElab/ElabTopdecl.sml and
                Compiler/Regions/DropRegions.sml *)
   end = List

 end
