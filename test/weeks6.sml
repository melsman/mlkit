(*
From: Stephen Weeks <sweeks@sweeks.com>
Subject: [Sml-implementers] type-checking bug in several implementations
To: sml-implementers@lists.sourceforge.net
Date: Fri, 9 Apr 2004 11:38:22 -0700


The following program is incorrectly handled by Hamlet, MLton, the ML
Kit, and Moscow ML.  It should be rejected as type incorrect.  Hamlet
and Moscow ML compile the program, but encounter a run time error (a
seg fault in the case of Moscow ML).  MLton and the ML Kit report
internal errors at compile time.

I suspect all four suffer from the same defect in the type checker,
which is too lax and concludes that a monotype containing an
unspecified type generalizes a type scheme.

*)

structure S :
   sig
      val f: 'b option -> 'b option
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
(*
val _ = S.f (SOME 13)

val _ =
   case S.f (SOME (fn z => z)) of
      NONE => 15
    | SOME f => f 17
*)