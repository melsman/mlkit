(*$CRASH*)

(* CRASH signature: used for internal consistency errors and so on. *)
signature CRASH =
  sig
    val assert: (string * bool) -> unit
    val impossible: string -> 'a
    val unimplemented: string -> 'a

    exception CRASH			(* So we can catch it and reenter
					   at top-level. *)
  end;
