(* rand-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a simple random number generator.
 *
 *)

signature RAND =
  sig

    val randMin : real
    val randMax : real
    val random : real -> real
      (* Given seed, return value randMin <= v <= randMax
       * Iteratively using the value returned by random as the
       * next seed to random will produce a sequence of pseudo-random
       * numbers.
       *)

    val mkRandom : real -> unit -> real
      (* Given seed, return function generating a sequence of
       * random numbers randMin <= v <= randMax
       *)

    val norm : real -> real
      (* r -> r / (randMax + 1.0) *)

    val range : (int * int) -> real -> int 
      (* Map v, randMin <= v <= randMax to integer range [i,j]
       * Exception -
       *   BadArg if j < i
       *)

  end (* RAND *)
