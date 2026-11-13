(* Nested FP result joins preserve branch selection, effects and exceptions. *)
local
  exception Missing
  exception Value of int
  datatype input = Number of real | Borrowed of real | Absent
  val effects = ref 0
  fun check true = ()
    | check false = raise Fail "FP result join"
  fun nested c =
    (case (effects := !effects + 1; c) of
       Number x =>
         let val () = effects := !effects + 10
         in if x > 0.0 then Math.sqrt x else ~0.0
         end
     | Borrowed x => x
     | Absent => raise Missing) * 1.0
  fun ints n = (case n of 0 => 1.5 | 1 => 2.5 | _ => raise Missing) * 2.0
  fun words w = (case w of 0w0 => 1.5 | 0w1 => 2.5 | _ => raise Missing) * 2.0
  fun strings s = (case s of "a" => 1.5 | "b" => 2.5 | _ => raise Missing) * 2.0
  fun exceptions e =
    (case e of Missing => 1.5 | Value n => Real.fromInt n | _ => raise e) * 2.0
  fun handled n =
    (case n of 0 => ((raise Missing) handle Missing => 3.0)
             | _ => Math.sqrt 16.0) * 2.0
  val () = check (Real.==(nested (Number 9.0), 3.0) andalso !effects = 11)
  val () = check (Real.signBit (nested (Number (~1.0))) andalso !effects = 22)
  val () = check (Real.==(nested (Borrowed 7.0), 7.0) andalso !effects = 23)
  val () = check ((nested Absent; false) handle Missing => !effects = 24)
  val () = check (Real.isNan (nested (Borrowed (0.0 / 0.0))))
  val () = List.app (fn f =>
      (check (Real.==(f 0, 3.0)); check (Real.==(f 1, 5.0));
       check ((f 2; false) handle Missing => true)))
      [ints, fn n => words (Word.fromInt n),
       fn n => strings (case n of 0 => "a" | 1 => "b" | _ => "c")]
  val () = check (Real.==(exceptions Missing, 3.0))
  val () = check (Real.==(exceptions (Value 4), 8.0))
  val () = check (Real.==(handled 0, 6.0) andalso Real.==(handled 1, 8.0))
in
  val () = print "FP result joins passed\n"
end
