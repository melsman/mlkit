(* Arraysort.sml -- adapted for Moscow ML from SML/NJ library v. 0.2
 *               -- ported from Moscow ML, 2001-06-07
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 * See file mosml/copyrght/copyrght.att for details.
 *
 * Structure for in-place sorting of arrays.
 * Uses an engineered version of quicksort due to 
 * Bentley and McIlroy.
 *
 *)

structure Arraysort : ARRAYSORT =
  struct

    open Array

    fun min (x, y) = if x < y then x else y : int;

    fun sortRange (array, start, n, cmp) = let
	  fun swap i j = 
	      let val tmp = sub (array, i)
	      in update (array, i, (sub (array, j))); update(array,j,tmp) end
	  fun vecswap i j 0 = ()
	    | vecswap i j n = (swap i j; vecswap (i+1) (j+1) (n-1))
	  fun insertSort (start, n) = let
		val limit = start+n
		fun outer i =
		      if i >= limit then ()
		      else let
			fun inner j =
			      if j = start then outer(i+1)
			      else let
				val j' = j - 1
				in
				  if cmp(sub (array, j'), sub(array, j)) = GREATER
				    then (swap j j'; inner j')
				    else outer(i+1)
				end
			in inner i end
		in
		  outer (start+1)
		end

	  fun med3 a b c =
		case (cmp(sub(array, a), sub(array, b)),
		      cmp(sub(array, b), sub(array, c))) of
		  (LESS,LESS) => b
		| (LESS, _  ) =>
		      (case cmp(sub(array, a), sub(array, c)) of LESS => c | _ => a)
		| (_,GREATER) => b
		|  _          => 
		      (case cmp(sub(array, a), sub(array, c)) of LESS => a | _ => c)

	  fun getPivot (a,n) = 
		if n <= 7 then a + n div 2
		else let
		  val p1 = a
		  val pm = a + n div 2
		  val pn = a + n - 1
		  in
		    if n <= 40 then med3 p1 pm pn
		    else let
		      val d = n div 8
		      val p1 = med3 p1 (p1+d) (p1+2*d)
		      val pm = med3 (pm-d) pm (pm+d)
		      val pn = med3 (pn-2*d) (pn-d) pn
		      in
			med3 p1 pm pn
		      end
		  end

	  fun quickSort (arg as (a, n)) = let
		fun bottom limit = let
		      fun loop pa pb =
			    if pb > limit then (pa, pb)
			    else case cmp(sub(array, pb), sub(array, a)) of
			      GREATER => (pa, pb)
			    | LESS => loop pa (pb+1)
			    | _ => (swap pa pb; loop (pa+1) (pb+1))
		      in loop end

		fun top limit = let
		      fun loop pc pd =
			    if limit > pc then (pc, pd)
			    else case cmp(sub(array, pc), sub(array, a)) of
			      LESS => (pc, pd)
			    | GREATER => loop (pc-1) pd
			    | _ => (swap pc pd; loop (pc-1) (pd-1))
		      in loop end

		fun split pa pb pc pd = 
		    let val (pa,pb) = bottom pc pa pb
			val (pc,pd) = top pb pc pd
		    in
			if pb > pc then (pa,pb,pc,pd)
			else (swap pb pc; split pa (pb+1) (pc-1) pd)
		    end

		val pm = getPivot arg
		val _ = swap a pm
		val pa = a + 1
		val pc = a + (n-1)
		val (pa,pb,pc,pd) = split pa pa pc pc
		val pn = a + n
		val r = min(pa - a, pb - pa)
		val _ = vecswap a (pb-r) r
		val r = min(pd - pc, pn - pd - 1)
		val _ = vecswap pb (pn-r) r
		val n' = pb - pa
		val _ = if n' > 1 then sort(a,n') else ()
		val n' = pd - pc
		val _ = if n' > 1 then sort(pn-n',n') else ()
		in () end

	  and sort (arg as (_, n)) = if n < 7 then insertSort arg 
				     else quickSort arg
	  in sort (start,n) end

    fun sort cmp (arr : 'a array) = 
	sortRange(arr, 0, length arr, cmp)

    fun sorted cmp (arr : 'a array) = 
	let val len = length arr
	    val array = arr
	    fun s v1 v2 i = 
		(* s[0..i-2] is sorted & v1 = s[i-2] & v2 = s[i-1] *)
		cmp(v1, v2) <> GREATER
		andalso (i >= len orelse s v2 (sub(array, i)) (i+1))
	in
	    len = 0 orelse len = 1 orelse s (sub(array, 0)) (sub(array, 1)) 2
	end
  end
