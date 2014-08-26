(* File time.sml: Testing Time operations.
 * Copyright (c) 1995-2014, Peter Sestoft, Martin Elsman.
 * MIT License.
 *)

open Utest
val _ = tstStart "structure Time"

local 
    fun fib n = if n<2 then 1 else fib(n-1) + fib(n-2)
    open Time
    val bigt = fromSeconds 87654321 + fromMicroseconds 500012
    val litt = fromSeconds 454 + fromMicroseconds 501701

val test1 = 
    tst' "test1" (fn _ => zeroTime + bigt = bigt andalso bigt - zeroTime = bigt)

val test2a = 
    tst' "test2a" (fn _ => toSeconds zeroTime = 0
           andalso zeroTime = fromSeconds 0
           andalso zeroTime = fromMilliseconds 0
           andalso zeroTime = fromMicroseconds 0)
val test2b = 
    tst' "test2b" (fn _ => toSeconds bigt = 87654321
           andalso toSeconds litt = 454
           andalso toMilliseconds litt = 454501
           andalso toMicroseconds litt = 454501701)
val test2c = 
    tst' "test2c" (fn () => (fromSeconds ~1; false)
                            handle Time => true)
val test2d =
    tst' "test2d" (fn () => (fromMilliseconds ~1; false)
                            handle Time => true)
val test2e =
    tst' "test2e" (fn () => (fromMicroseconds ~1; false)
                            handle Time => true)

val test3a = 
    tst' "test3a" (fn _ => fromReal 0.0 = zeroTime
                   andalso fromReal 10.25 = fromSeconds 10 + fromMilliseconds 250);
val test3b =
    tst' "test3b" (fn () => (fromReal ~1.0; false)
                            handle Time => true)
val test3c =
    tst' "test3c" (fn () => (fromReal 1E300; false)
                            handle Time => true)

val test4a = 
    tst' "test4a" (fn _ => Real.==(toReal (fromReal 100.25), 100.25))

val test6a = 
    tst' "test6a" (fn _ => bigt + litt = litt + bigt
                   andalso (bigt + litt) - litt = bigt
                   andalso (bigt - litt) + litt = bigt)

val test7a = 
    tst' "test7a" (fn _ => litt <= litt andalso litt >= litt
           andalso zeroTime < litt andalso litt > zeroTime
           andalso litt < bigt andalso bigt > litt
           andalso not (litt > bigt) 
           andalso not (bigt < litt) 
           andalso not(litt < litt)
           andalso not(litt > litt))

val test8a = 
    tst' "test8a" (fn _ => now() <= now() 
                           andalso (now () before (fib 23; ())) < now())

val test9a = 
    tst' "test9a" (fn _ => fmt ~1 litt  = "455"
           andalso fmt 0 litt = "455")

val test9b = 
    tst' "test9b" (fn _ => fmt 1 litt = "454.5"
           andalso fmt 2 litt = "454.50"
           andalso fmt 3 litt = "454.502"
           andalso fmt 4 litt = "454.5017"
           andalso fmt 5 litt = "454.50170"
           andalso fmt 6 litt = "454.501701")
    
fun chk (s, r) = 
    tst' "test10a" (fn _ => 
           case fromString s of
               SOME res => res = fromMicroseconds r
             | NONE     => false)

val test10a =
    List.map chk
         [("189", 189000000),
          ("189.1", 189100000),
          ("189.125125", 189125125),
          (".1", 100000),
          (".125125", 125125),
          (" \n\t189crap", 189000000),
          (" \n\t189.1crap", 189100000),
          (" \n\t189.125125crap", 189125125),
          (" \n\t.1crap", 100000),
          (" \n\t.125125crap", 125125)]

val test10b = 
    List.app (fn s => tst "test10b" (case fromString s of NONE => true | _ => false))
         ["", "+189", "~189", "now", "Monday"]
in

val _ = tstEnd()
end
