(*array.sml  31/07/1997 16:07. tho.*)

(*attempt at implementing arrays in the ml kit*)

(*stupid implementation of N-arrays.
  supposed to be primitive in the kit (N will be something like the size
				       of a memory chunk).*)
(*fun flush os = Outstream.flush os*)
fun flush os = ()
fun pr (s : string) : unit = (output (std_out, s);
			      flush std_out)
local
  fun digit n = chr(ord #"0" + n)
  fun digits(n,acc) =
        if n >=0 andalso n<=9 then digit n:: acc
	else digits (n div 10, digit(n mod 10) :: acc)
in
  fun int_to_string n = implode(digits(n,[]))
end (*local*)

exception Impossible
fun impossible s = impossible0 (s ^ "\n")
and impossible0 s = (pr ("\nimpossible: " ^ s); raise Impossible)

structure prim = struct
  type 'x array = 'x ref list
  val impossible = fn s => impossible ("prim." ^ s)
  val maxLen = 1
  fun array (n, x : int) : int array = if n<>maxLen then impossible "array"
				       else array0 n x
  and array0 0 x = []
    | array0 n x = ref x :: array0 (n-1) x

  fun check (xrs, i) = if 0<=i andalso i<maxLen then ()
		       else impossible " subscript out of range"
  fun map f (xrs : 'x array, i) = (check (xrs, i); map0 f (xrs, i))
  and map0 f ([],i) = impossible "map0"
    | map0 f (xr::xrs,0) = f xr
    | map0 f (xr::xrs,i) = map0 f (xrs,i-1)
  fun update (xsr : 'x array, i, x) = map (fn xr => xr := x) (xsr, i)
  val sub = map op !
end (*structure prim*)

(*should be declared by structure General:*)
exception Size
exception Subscript

(*implementation of (general) arrays using the ``primitive''
 N-arrays above*)

structure Array = struct
  val impossible = fn s => impossible ("Array." ^ s)
  datatype 'x array0 = Lf | Br of 'x array0 * 'x prim.array * 'x array0
  (*`pa' is a prim.array;
   `pi' is an index in a prim.array (an int);
   `path' is an int which, viewed as binary number, represents a path in the
   binary tree that represents an array;
   `a' : 'x array;
   `a0' : 'x array0;
   `x' : 'x ---an element in an array;
   `i' : int ---an index;
   `n' : int ---the number of elements in some array (which, then, has the
          legal indexes 0, ..., n-1);

   an index i into a0 : 'x array0 may be thought of as a pair (path, pi)
   consisting of a path in the binary tree a0, and an index pi into a
   prim.array.  (path, pi) = (i div prim.maxLen, i mod prim.maxLen).*)
    
  type 'x array = (int (*max index*) * 'x array0) ref

  val maxLen = 123456789 (*no good reason*)
  fun check (n, i) = if 0<=i andalso i<n then () else raise Subscript
  fun check_size n = if 0<=n andalso n<=maxLen then () else raise Size
				     
  (*map f (a, i) = find the prim.array pa and `prim-index' pi in array a that
   contains the entry indexed i, then apply f to (pa, pi).  For instance,
   map prim.sub is array.sub.  raise Subscript if i is out of range in a.*)

  fun map f (a, i) =
        (case !a of (n, a0) => (check (n, i); map0 f (a0, i)))
  and map0 f (a0, i) = map00 f (a0, (i div prim.maxLen + 1, i mod prim.maxLen))
  and map00 f (Br (a00, pa, a01), (path, pi)) =
        if path = 1 then (*we are there*) f (pa, pi)
	else map00 f (if path mod 2 = 0 then a00 else a01, (path div 2, pi))
    | map00 f (Lf, (path, pi)) = impossible ("map00")

  val sub = map prim.sub
  fun update (a, i, x) = map (fn (pa, pi) => prim.update (pa, pi, x)) (a, i)
	  
  fun array (n, x) = (check_size n; ref (n, array0 (n, x)))
  and array0 (n, x) = array00 (1, 1, ~(~n div prim.maxLen), x)
        (*The number of prim arrays needed for an array of size n is not
	 n div prim.maxLen, rather it is ~(~n div prim.maxLen).*)
  and array00 (path, next, m, x) =
        if path > m then Lf
	else Br (array00 (path + next, next * 2, m, x),
		 prim.array (prim.maxLen, x),
		 array00 (path + next + 1, next * 2, m, x))
  val nil_array = ref (0, Lf)

  fun fromList [] = nil_array (*we can't call array here, as we have no
			       init element to pass to it*)
    | fromList (x::xs) =
        let val n = List.length xs
	    val a = if n > maxLen then raise Size else array (n, x)
	    fun init ([], i) = ()
	      | init (x::xs, i) = (update (a, i, x); init (xs, i+1))
	in (init (xs, 1); a)
	end

  fun tabulate (n, f) =
        (check_size n;
	 let val a = array (n, f 0)
	     fun init i = if i >= n then ()
			  else (update (a, i, f i); init (i+1))
	 in (init 1; a)
	 end)

  fun length (ref (n, a0)) = n

  fun valid (a, i, SOME n) = 0 <= i andalso i <= i+n andalso i+n <= length a
    | valid (a, i, NONE)   = 0 <= i andalso i <=                    length a
  fun check_slice slice = if valid slice then () else raise Subscript
  fun a_from_to (a, i, n_opt) = (a, i, (case n_opt of
					  SOME n => i+(n-1)
					| NONE => length a + 1))
  fun extract (a, i, n_opt) =
        let val l = length a
	    val n = (case n_opt of NONE => l - i | SOME n => n)
	    val a' = if i<0 orelse n<0 orelse i+n>l then raise Subscript
		     else array (n, sub (a, i))
	    fun copy j = if j<n then (update (a', j, sub (a, (i+j)));
				      copy (j+1))
			 else ()
	in
	  (copy 1; a')
	end

  fun copy {src, si, len, dst, di} =
        let val n = (case len of NONE => length src - si | SOME n => n)
	in
	  if n<0 orelse si<0 orelse si+n > length src
	         orelse di<0 orelse di+n > length dst then raise Subscript
	  else if si < di then		(* copy from high to low *)
	         let fun hdilo j =
		           if j >= 0 then
			     (update (dst, di+j, sub (src, si+j)); hdilo (j-1))
			   else ()
		 in hdilo (n-1)
		 end
	       else                       (* si >= di, copy from low to high *)
		 let fun lo2hi j =
		           if j < n then
			     (update (dst, di+j, sub (src, si+j)); lo2hi (j+1))
			   else ()
		 in lo2hi 0
		 end
	end
(*
  fun copyVec {src=a1: 'a vector, si=i1, len, dst=a2: 'a array, di=i2} =
        let val a2 = from_array a2
	    val n = case len of NONE => lengthv_ a1 - i1 | SOME k => k
	in
	  if n<0 orelse i1<0 orelse i1+n > lengthv_ a1
	    orelse i2<0 orelse i2+n > length_ a2
	    then
		raise Subscript
	  else
	    let fun lo2hi j = if j < n then
	      (update_ a2 (i2+j) (subv_ a1 (i1+j)); lo2hi (j+1))
			      else ()
	    in lo2hi 0 end
	end;
*)



  fun app f a =
        let val stop = length a
	    fun lr j = if j < stop then (f (sub (a, j)); lr (j+1)) else ()
	in lr 0
	end

  fun sliceend (a, i, NONE) =
        if i<0 orelse i>length a then raise Subscript
	else length a
    | sliceend (a, i, SOME n) =
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

  fun appi f (slice as (a, i, _)) =
        let val stop = sliceend slice
	    fun lr j =
	          if j < stop then (f (j, sub (a, j)); lr (j+1)) else ()
	in lr i
	end

  fun foldli f e (slice as (a, i, _)) =
        let val stop = sliceend slice
	    fun lr (j, res) =
	          if j < stop then lr (j+1, f (j, sub (a, j), res)) else res
	in lr (i, e)
	end

  fun foldri f e (slice as (a, i, _)) =
        let fun rl (j, res) = if j >= i then rl (j-1, f (j, sub (a, j), res))
			      else res
	in rl (sliceend slice - 1, e)
	end

  fun foldl f e a =
        let val stop = length a
	    fun lr (j, res) = if j < stop then lr (j+1, f (sub (a, j), res))
			      else res
	in lr (0, e)
	end

  fun foldr f e a =
        let fun rl (j, res) = if j >= 0 then rl (j-1, f (sub (a, j), res))
			      else res
	in rl (length a - 1, e)
	end

  fun modifyi f (slice as (a, i, _)) =
        let val stop = sliceend slice
	    fun lr j =
	          if j < stop then (update (a, j, f (j, sub (a, j))); lr (j+1))
		  else ()
	in lr i
	end

  fun modify f a =
        let val stop = length a
	    fun lr j = if j < stop then (update (a, j, f (sub (a, j)));
					 lr (j+1))
		       else ()
	in lr 0
	end
end (*structure Array*)
;
