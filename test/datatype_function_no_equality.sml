(* A reference alternative must not hide a non-equality alternative. *)
datatype t = R of (int -> int) ref | F of (int -> int)
val r = R (ref (fn x:int => x))
val bad = r = r
