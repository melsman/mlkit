(* File general.sml: Testing General operations.
 * Copyright (c) 1994-2014, Peter Sestoft, Martin Elsman.
 * MIT License.
 *)

open Utest
val _ = tstStart "structure General"

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

fun checkExn(exnStr, exn) = tst "checkExn" (exnStr = exnName exn)

exception E1
exception E2 = E1

val _ = List.app checkExn
    [("E1",        E2),
     ("Bind",      getExn(fn _ => let val true = false in () end)),
     ("Match",     getExn(fn _ => (fn true => ()) false)),
     ("Subscript", getExn(fn _ => Vector.sub(vector [], ~1))),
     ("Size",      getExn(fn _ => Array.array(Array.maxLen+1, ()))),
     ("Div",       getExn(fn _ => 1 div 0)),
     ("Chr",       getExn(fn _ => Char.chr 9999999)),
     ("Fail",      Fail "demo"),
     ("Option",    getExn(fn _ => valOf NONE)),
     ("Empty",     getExn(fn _ => List.hd []))
    ]

val () = tstEnd()
