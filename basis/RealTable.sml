signature BLOCK_REAL = sig
  type t
  val alloc  : int -> t
  val update : t * int * real -> unit
  val sub    : t * int -> real
  val length : t -> int
  val maxLen : int
end

structure BlockReal : BLOCK_REAL = struct
type t = chararray
fun alloc (i:int) : t = prim("allocStringML", (8*i))
fun update (t:t,i:int,v:real) : unit =
    prim("__blockf64_update_real", (t,i,v))
fun sub (t:t,i:int) : real =
    prim("__blockf64_sub_real", (t,i))
fun length (t:t) : int =
    prim ("__blockf64_size", t)
val maxLen = 12345678 (* arbitrarily chosen *)
end

structure RealTable = struct

structure B = BlockReal
type elem = real
type vector = B.t
type array = B.t

val maxLen = B.maxLen
val length = B.length

fun check_index (n, i) =
    if 0 <= i andalso i < n then ()
    else raise Subscript

local
  fun check_size n =
      if 0 <= n andalso n <= maxLen then ()
      else raise Size
in
fun alloc_table n =
    (check_size n; B.alloc n)
end

fun sub (t:B.t,i:int) : elem =
    (check_index(length t, i); B.sub(t,i))

fun update (t:B.t,i:int,x:real) : unit =
    (check_index(length t, i); B.update(t,i,x))

fun fromList (xs : real list) : B.t =
    let fun init (t, [], i) = t
	  | init (t, x::xs, i) = (B.update (t, i, x); init (t, xs, i+1))
        val n = List.length xs
    in init (alloc_table n, xs, 0)
    end

fun tabulate (n, f : int -> real) : B.t =
    let fun init (t, f, i) = if i >= n then t
			     else (B.update (t, i, f i); init (t, f, i+1))
    in init (alloc_table n, f, 0)
    end

fun vector a = tabulate (length a, fn i => B.sub(a,i))

(* array n gives an initialised array with n elements. *)
(* Raise Size if n > maxLen.                           *)
fun array (n,x) =
    let val a = alloc_table n
        fun lr j =
	    if j < n then (B.update (a,j,x);
                           lr (j+1))
             else ()
    in lr 0; a
    end

fun updatev (t, i, x) =
    (check_index(length t, i);
     tabulate (length t,fn j => if i=j then x else B.sub(t,j)))

fun copy {src=a1:B.t, dst=a2:B.t, di=i2} =
    let val n = length a1
    in if i2<0 orelse i2+n > length a2 then
	 raise Subscript
       else let fun hi2lo j =       (* copy from high to low *)
		    if j >= 0 then
		      (B.update(a2,i2+j,B.sub(a1,j));
                       hi2lo (j-1))
		    else ()
	    in hi2lo (n-1)
	    end
    end

fun copyVec {src=v:B.t, dst=a:B.t, di=i2} =
    let val n = length v
    in if i2<0 orelse i2+n > length a then
	 raise Subscript
       else let fun lo2hi j =
		    if j < n then
		      (B.update(a,i2+j,B.sub(v,j));
                       lo2hi (j+1))
		    else ()
	    in lo2hi 0
	    end
    end

fun app f a =
    let val n = length a
        fun lr j =
	    if j < n then (f (B.sub (a, j)):unit;
                           lr (j+1))
	    else ()
    in lr 0
    end

fun foldli f e a =
    let val stop = length a
	fun lr j res =
	    if j < stop then lr (j+1) (f(j, B.sub(a,j), res))
	    else res
    in lr 0 e
    end

fun foldri f e a =
    let fun rl j res =
	    if j >= 0 then rl (j-1) (f(j, B.sub(a,j), res))
	    else res
    in rl (length a - 1) e
    end

fun appi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (f(j, B.sub(a,j)):unit;
                              lr (j+1))
	    else ()
    in lr 0
    end

fun mapi (f : int * elem -> elem) (a : B.t) : B.t =
    let val stop = length a
	val newvec = B.alloc stop  (* no check_size needed *)
	fun lr j =
	    if j < stop then
	      (B.update(newvec, j, f(j, B.sub(a,j)));
	       lr (j+1))
	    else ()
    in lr 0; newvec
    end

fun foldl f e a =
    let val n = length a
	fun lr (j, res) =
	    if j < n then lr (j+1, f (B.sub (a, j), res))
	    else res
    in lr (0, e)
    end

fun foldr f e a =
    let val n = length a
        fun rl (j, res) =
	    if j >= 0 then rl (j-1, f (B.sub (a, j), res))
	    else res
    in rl (n-1, e)
    end

fun modifyi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (B.update(a,j,f(j, B.sub(a,j)));
                              lr (j+1))
	    else ()
    in lr 0
    end

fun modify f a =
    let val n = length a
        fun lr j =
	    if j < n then (B.update (a, j, f (B.sub (a, j)));
                           lr (j+1))
	    else ()
    in lr 0
    end

(* The following are only for the Vector structure: *)
fun map (f : elem -> elem) (a : B.t) : B.t =
    let val n = length a
        val b : B.t = B.alloc n   (* no check_size needed *)
	fun lr j =
	    if j < n then (B.update (b, j, f (B.sub (a, j)));
                           lr (j+1))
	    else b
    in lr 0
    end

fun mapi (f : int * elem -> elem) (a : B.t) : B.t =
    let val stop = length a
	val newvec = B.alloc stop  (* no check_size needed *)
	fun lr j =
	    if j < stop then
	      (B.update(newvec, j, f(j, B.sub(a,j)));
	       lr (j+1))
	    else ()
    in lr 0; newvec
    end

fun concat (vecs : B.t list) =
    let fun total_length ([], n) = n
	  | total_length (v::vs, n) = total_length (vs, length v + n);
        val n = total_length (vecs, 0)
        val v = alloc_table n
        fun copyall (to, []) = v
	  | copyall (to, x :: xs) =
	    let val x_n = length x
	        fun copy j =
	            if j < x_n then (B.update (v, to+j, B.sub (x, j));
                                     copy (j+1))
	            else ()
	    in copy 0; copyall (to+x_n, xs)
	    end
    in copyall (0, vecs)
    end

fun findi (p : int * elem -> bool) (a : B.t) : (int * elem) option =
    let val stop = length a
	fun lr j =
	    if j < stop then
	      if p (j, B.sub(a,j)) then SOME (j, B.sub(a,j)) else lr (j+1)
	    else NONE
    in lr 0
    end

fun find (p : elem -> bool) (a : B.t) : elem option =
    let val stop = length a
	fun lr j =
	    if j < stop then
	      if p (B.sub(a,j)) then SOME (B.sub(a,j))
	      else lr (j+1)
	    else NONE
    in lr 0
    end

fun exists (p : elem -> bool) (a : B.t) : bool =
    let val stop = length a
	fun lr j = j < stop andalso (p (B.sub(a,j)) orelse lr (j+1))
    in lr 0
    end

fun all (p : elem -> bool) (a : B.t) : bool =
    let val stop = length a
	fun lr j = j >= stop orelse (p (B.sub(a,j)) andalso lr (j+1))
    in lr 0
    end

fun collate cmp (v1, v2) =
    let val n1 = length v1
	and n2 = length v2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point v1[0..j-1] = v2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
	      case cmp(B.sub(v1,j), B.sub(v2,j)) of
		  EQUAL => h (j+1)
		| res   => res
    in h 0
    end
end
