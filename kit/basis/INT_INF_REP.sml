(* Internal representation of IntInf.int including conversion
   functions to be used in the Int/IntN/Word/WordN
   implementations. This signature, as well as its matching structure
   is declared before any of the Int/IntN/Word/WordN modules. mael
   2005-12-14 *)

signature INT_INF_REP =
  sig
      type intinf
	      
      val fromInt    : int -> intinf
      val toInt      : intinf -> int

      val fromInt31  : int31 -> intinf
      val toInt31    : intinf -> int31

      val fromInt32  : int32 -> intinf
      val toInt32    : intinf -> int32

      val fromWord   : word -> intinf
      val fromWordX  : word -> intinf
      val toWord     : intinf -> word

      val fromWord8  : word8 -> intinf
      val fromWord8X : word8 -> intinf
      val toWord8    : intinf -> word8

      val fromWord31 : word31 -> intinf
      val fromWord31X : word31 -> intinf
      val toWord31   : intinf -> word31

      val fromWord32 : word32 -> intinf
      val fromWord32X : word32 -> intinf
      val toWord32   : intinf -> word32

      (* for overloading *)
      val +   : intinf * intinf -> intinf
      val -   : intinf * intinf -> intinf
      val *   : intinf * intinf -> intinf
      val ~   : intinf -> intinf
(*
      val div : intinf * intinf -> intinf
      val mod : intinf * intinf -> intinf
      val abs : intinf -> intinf
      val <   : intinf * intinf -> intinf
      val >   : intinf * intinf -> intinf
      val <=  : intinf * intinf -> intinf
      val >=  : intinf * intinf -> intinf
*)
  end