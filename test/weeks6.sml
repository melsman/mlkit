structure S:
   sig
      val f: 'a option -> 'a option
   end =
   struct
      val make: unit -> 'a option -> 'a option =
	 fn () =>
	 let
	    val r: 'a option ref = ref NONE
	 in
	    fn z => (!r before (r := z))
	 end
      val f = make ()
   end

val _ = S.f (SOME 13)

val _ =
   case S.f (SOME (fn z => z)) of
      NONE => 15
    | SOME f => f 17
