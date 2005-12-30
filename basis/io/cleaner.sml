(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Cleaner: CLEANER =
struct

type t = (unit -> unit) list ref

val atExit = Initial.clearnerAtExit

fun clean cs = app (fn c => c () handle _ => ()) (!cs)

fun addNew (cs, f) =
  ((if !Initial.addedclearner
    then () 
    else (OS.Process.atExit (fn () => clean atExit) ; Initial.addedclearner := true))
    ; cs := f :: (!cs))

end
