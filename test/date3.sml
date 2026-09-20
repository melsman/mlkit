(* strftime answers 0 both when the result does not fit and when it is
 * legitimately empty, as it is for an empty format string.  MLKit used to
 * take the empty result for a failure and raise; the raise then passed the
 * exception value through an `int` parameter of sml_strftime, truncating the
 * pointer and crashing the program. *)

val d = Date.date {year = 2000, month = Date.Jan, day = 2, hour = 9,
                   minute = 5, second = 7, offset = SOME Time.zeroTime}

fun show f = (Date.fmt f d) handle e => "exn " ^ exnName e

val () = app (fn (f, want) =>
                 let val got = show f
                 in print (if got = want then "Ok\n"
                           else "Error: fmt \"" ^ String.toString f
                                ^ "\" = \"" ^ String.toString got
                                ^ "\", expected \"" ^ String.toString want ^ "\"\n")
                 end)
             [("", ""),
              ("%Y", "2000"),
              ("%%", "%"),
              ("x%Yy", "x2000y")]
