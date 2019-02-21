(* Random -- Random number generator -- 1995-04-23 *)

signature RANDOM =
  sig
    type generator

    val newgenseed : real -> generator
    val newgen     : unit -> generator
    val random     : generator -> real
    val randomlist : int * generator -> real list
    val range      : int * int -> generator -> int
    val rangelist  : int * int -> int * generator -> int list
  end

(* Type generator is the abstract type of random number generators,
   producing uniformly distributed pseudo-random numbers.

   [newgenseed seed] returns a random number generator with the given seed.

   [newgen ()] returns a random number generator, taking the seed from
   the system clock.

   [random gen] returns a random number in the interval [0..1).

   [randomlist (n, gen)] returns a list of n random numbers in the
   interval [0,1).

   [range (min, max) gen] returns an integral random number in the
   range [min, max).  Raises Fail if min > max.

   [rangelist (min, max) (n, gen)] returns a list of n integral random
   numbers in the range [min, max).  Raises Fail if min > max.
*)

structure Random : RANDOM =
  struct

    (* Generating random numbers.  Paulson, page 96 *)

    fun getrealtime () : {sec : int, usec : int} =
        {sec=1,usec=1}

    type generator = {seedref : real ref}

    val a = 16807.0
    val m = 2147483647.0
    fun nextrand seed = let val t = a*seed
			in t - m * real(floor(t/m))
			end

    fun newgenseed seed = {seedref = ref (nextrand seed)}

    fun newgen () =
      let val {sec, usec} = getrealtime ()
      in newgenseed (real sec + real usec)
      end

    fun random {seedref as ref seed} = (seedref := nextrand seed; seed / m)

    fun randomlist (n, {seedref as ref seed0}) =
      let fun h 0 seed res = (seedref := seed; res)
	    | h i seed res = h (i-1) (nextrand seed) (seed / m :: res)
      in h n seed0 []
      end

    fun range (min, max) =
      if min >= max then raise Fail "Random.range: empty range"
      else fn {seedref as ref seed} =>
	   (seedref := nextrand seed; min + (floor(real(max-min) * seed / m)))

    fun rangelist (min, max) =
      if min >= max then raise Fail "Random.rangelist: empty range"
      else fn (n, {seedref as ref seed0}) =>
	   let fun h 0 seed res = (seedref := seed; res)
		 | h i seed res = h (i-1) (nextrand seed)
	                    (min + floor(real(max-min) * seed / m) :: res)
	   in h n seed0 []
	   end
  end

fun merge(xs, []):int list = xs
  | merge([], ys) = ys
  | merge(l1 as x::xs, l2 as y::ys) =
    if x<y then x :: merge(xs, l2)
    else y :: merge(l1, ys)

(* splitting a list *)
fun split(x::y::zs, l, r) = split(zs, x::l, y::r)
  | split([x], l, r) = (x::l, r)
  | split([], l, r) = (l, r)

fun msort []  = []
  | msort [x] = [x]
  | msort xs = let val (l, r) = split(xs, [], [])
               in merge(msort l, msort r)
               end

fun upto n =
    let fun loop(p as (0,acc)) = p
          | loop(n, acc) = loop(n-1, n::acc)
    in #2(loop(n,[]))
    end

val result = print "Generating random numbers...\n"

val nums = Random.rangelist (0,100000) (1000000,Random.newgen())

val result = print "Sorting...\n"

val runmsort = msort nums

val result = print "Done.\n"
