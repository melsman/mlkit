(* rand.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details
 *
 * Random number generator taken from Paulson, pp. 170-171.
 * Recommended by Stephen K. Park and Keith W. Miller, 
 * Random number generators: good ones are hard to find,
 * CACM 31 (1988), 1192-1201
 *
 * Note: The Random structure provides a better generator.
 *)

structure Rand : RAND =
  struct

  (* real number version for systems with 46-bit mantissas *)
    val a = 16807.0  and  m = 2147483647.0

    val randMin = 1.0
    val randMax = m - 1.0

    fun random seed = let 
          val t = a*seed
          in
            t - m * real(floor(t/m))  
          end

    fun mkRandom seed = let
          val seed = ref seed
          in
            fn () => (seed := random (!seed); !seed)
          end

    fun norm r = r / m

    fun range (i,j) = 
          if j < i 
            then raise Fail "Rand.range"
            else let 
              val R = real(j - i + 1)
              in
                fn r => i + floor(R*(r/m))
              end handle _ => let
                val ri = real i
                val R = (real j)-ri+1.0
                in
                  fn r => floor(ri + R*(r/m))
                end

  end (* Rand *)

