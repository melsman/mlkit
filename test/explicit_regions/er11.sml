(* it is an error for a value in an explicit local
 * region to escape; here r must be live as long as
 * the closure can be called *)

fun f `[r] () : unit -> real =
  let with r
      val y = if true then (3.4,3)`r else (2.0,3)
  in fn () => if true then #1 y else 5.0
  end
