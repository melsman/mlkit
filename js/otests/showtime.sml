(* File showtime.sml: Simple functional reative programming example showing the time-of-day.
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

infix &
open Js.Element Rwp

val b = arr (Date.toString o Date.fromTimeLocal) (timer 100)

val e = taga "p" [("style","width:200;")] ($"")

val () = insertDOM_elem e b

val () = Dojo.runDialog "Time Rwp Example" e
