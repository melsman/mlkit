(* ---------------------------------------------------------------
   Structures for implementing unboxed monomorphic vectors, arrays,
   vector slices, array slices, and 2-dimensional arrays are grouped
   together to enforce abstractness of types but with shared knowledge
   about identies of types.
   --------------------------------------------------------------- *)

signature TABLES = sig
  structure V : MONO_VECTOR
  structure VS : MONO_VECTOR_SLICE where type elem = V.elem
                                     and type vector = V.vector
  structure A : MONO_ARRAY where type elem = V.elem
                                  and type vector = V.vector
  structure AS : MONO_ARRAY_SLICE where type elem = V.elem
                                    and type vector = V.vector
                                    and type array = A.array
                                    and type vector_slice = VS.slice
  structure A2 : MONO_ARRAY2 where type elem = V.elem
                               and type vector = V.vector
end

(* ---------------------------------------------------------------
   Functors for implementing unboxed monomorphic arrays, vectors,
   vector slices, array slices, and 2-dimensional arrays, with word
   values as the underlying element type, are parameterised by a
   structure that defines the size of elements in bytes and operations
   for accessing and updating elements, etc.
   --------------------------------------------------------------- *)

signature WORD_TABLE_ARG = sig
  type elem
  type table
  type vector = string
  type array = chararray
  val wordSizeBytes : int
  val tsub          : table * int -> elem
  val tupd          : table * int * elem -> unit
  val vsub          : vector * int -> elem
  val vupd          : vector * int * elem -> unit
  val asub          : array * int -> elem
  val aupd          : array * int * elem -> unit
  val tlen          : table -> int
  val vlen          : vector -> int
  val talloc        : int -> table
  val valloc        : int -> vector
  val aalloc        : int -> array
end

(* ---------------------------------------------------------------
   WordTable functor implementing monomorphic WordNVector and
   WordNArray structures.

   Internally, vectors are represented as strings and arrays as
   chararrays, which allows for the proper equality tests.

   The table type is defined in Runtime/Table.h.
 ----------------------------------------------------------------- *)

functor WordTable (Arg : WORD_TABLE_ARG) =
struct
  open Arg

  val length = tlen
  val length_vector = vlen

  (* quite a few bits are available for the length in the tag field! *)
  val maxLen = Initial.wordtable_maxlen

  fun chk_idx (n:int, i:int) : unit =
      if 0 <= i andalso i < n then ()
      else raise Subscript

  fun chk_sz (n:int) : unit =
      if 0 <= n andalso n <= maxLen then ()
      else raise Size

  fun sub (t:table, i:int) : elem =
      (chk_idx(length t, i); tsub(t,i))

  fun update (t:table, i:int, x:elem) : unit =
      (chk_idx(length t, i); tupd(t,i,x))

  (* table n returns an uninitialised table with n elements.
   * Raise Size if n > maxLen. *)
  fun table (n:int) : table =
      (chk_sz n; talloc n)

  (* vector n returns an uninitialised vector with n elements.
   * Raise Size if n > maxLen. *)
  fun vectorv n =
      (chk_sz n; valloc n)

  (* array n gives an initialised array with n elements. *)
  (* Raise Size if n > maxLen.                           *)
  fun array (n, x) =
      let val () = chk_sz n
          val a = aalloc n
          fun init i = if i < 0 then ()
                       else (aupd(a,i,x); init (i-1))
      in init (n-1)
       ; a
      end

  (* fromList creates a table from a list of elements *)
  fun fromList (xs : elem list) : table =
    let fun init (t, [], i) = t
	  | init (t, x::xs, i) = (tupd (t, i, x); init (t, xs, i+1))
        val n = List.length xs
    in init (table n, xs, 0)
    end

  (* tabulate creates a table of n elements *)
  fun tabulate (n, f : int -> elem) : table =
      let fun init f (t, i) = if i >= n then t
			      else (tupd (t, i, f i); init f (t, i+1))
      in init f (table n, 0)
      end

  fun tabulatev (n, f : int -> elem) : vector =
      let fun init f (t, i) = if i >= n then t
			      else (vupd (t, i, f i); init f (t, i+1))
      in init f (valloc n, 0)
      end

  fun vector (t:table) : vector =
      tabulatev (length t, fn i => tsub(t,i))

  fun updatev (t:table, i, x) : table =
      let val sz = length t
      in chk_idx(sz, i)
       ; tabulate (sz,fn j => if i=j then x else tsub(t,j))
      end

  fun copy {src=a1:table, dst=a2:table, di=i2} =
      let val n = length a1
      in if i2<0 orelse i2+n > length a2
         then raise Subscript
	 else let fun hi2lo j = (* copy from high to low *)
		      if j >= 0
                      then (tupd(a2, i2+j,tsub(a1,j)); hi2lo (j-1))
		      else ()
	      in hi2lo (n-1); ()
	      end
      end

  fun copyVec {src=v:vector, dst=a:table, di=i2} =
      let val n = length_vector v
      in if i2<0 orelse i2+n > length a
         then raise Subscript
	 else let fun lo2hi j =
		      if j < n then
			(tupd(a,i2+j,vsub(v,j)); lo2hi (j+1))
		      else ()
	      in lo2hi 0; ()
	      end
      end

  (* apply f on the elements from left to right. *)
  fun app f a =
      let val n = length a
          fun lr j =
	      if j < n then (f (tsub (a, j)) : unit; lr (j+1))
	      else ()
      in lr 0 ; ()
      end

  fun foldli f e a =
      let val stop = length a
	  fun lr (j, res, a) =
	      if j < stop then lr (j+1, f(j, tsub(a,j), res), a)
	      else res
      in lr (0, e, a)
      end

  fun foldri f e a =
      let fun rl (j, res, a) =
	      if j >= 0 then rl (j-1, f(j, tsub(a,j), res), a)
	      else res
      in rl (length a - 1, e, a)
      end

  fun appi f a =
      let val stop = length a
	  fun lr j =
	      if j < stop then (f(j, tsub(a,j)):unit; lr (j+1))
	      else ()
      in lr 0; ()
      end

  fun mapi (f : int * elem -> elem) (a : table) : table =
      let val stop = length a
	  val newvec = talloc stop
	  fun lr j =
	      if j < stop then
		(tupd(newvec, j, f(j, tsub(a,j)));
		 lr (j+1))
	      else ()
      in lr 0; newvec
      end

  fun foldl f e a =
      let val n = length a
	  fun lr (j, res, a) =
	      if j < n then lr (j+1, f (tsub (a, j), res), a)
	      else res
      in lr (0, e, a)
      end

  fun foldr f e a =
      let val n = length a
          fun rl (j, res, a) =
	      if j >= 0 then rl (j-1, f (tsub (a, j), res), a)
	      else res
      in rl (n-1, e, a)
      end

  fun modifyi f a =
      let val stop = length a
	  fun lr j =
	      if j < stop then (tupd(a,j,f(j, tsub(a,j))); lr (j+1))
	      else ()
      in lr 0; ()
      end

  fun modify f a =
      let val n = length a
          fun lr j =
	      if j < n then (tupd (a, j, f (tsub (a, j))); lr (j+1))
	      else ()
      in lr 0; ()
      end

  (* The following are only for the Vector structure: *)
  fun map (f : elem -> elem) (a:table) : table =
      let val n = length a
          val b : table = talloc n
	  fun lr j =
	      if j < n then (tupd (b, j, f (tsub (a, j))); lr (j+1))
	      else ()
      in lr 0; b
    end

  fun mapi (f : int * elem -> elem) (a:table) : table =
      let val stop = length a
	  val newtab = talloc stop
	  fun lr j =
	      if j < stop then
		(tupd(newtab, j, f(j, tsub(a,j)));
		 lr (j+1))
	      else ()
      in lr 0; newtab
      end

  fun concat (vecs : table list) =
      let fun total ([], n) = n
	    | total (v::vs, n) = total (vs, length v + n);
          val n = total (vecs, 0)
          val v : table = table n
          fun copyall (to, []) = v
	    | copyall (to, x :: xs) =
	      let val x_n = length x
	          fun copy j =
	              if j < x_n then (tupd (v, to+j, tsub (x, j)); copy (j+1))
	              else v
	      in copy 0; copyall (to+x_n, xs)
	      end
      in copyall (0, vecs)
      end

  fun findi (p : int * elem -> bool) (a : table) : (int * elem) option =
      let val stop = length a
	  fun lr j =
	      if j < stop then
		if p (j, tsub(a,j)) then SOME (j, tsub(a,j)) else lr (j+1)
	      else NONE
      in lr 0
      end

  fun find (p : elem -> bool) (a : table) : elem option =
      let val stop = length a
	  fun lr j =
	      if j < stop then
		if p (tsub(a,j)) then SOME (tsub (a,j))
		else lr (j+1)
	      else NONE
      in lr 0
      end

  fun exists (p : elem -> bool) (t:table) : bool =
      let val stop = length t
	  fun lr (j,t) = j < stop andalso (p (tsub(t,j)) orelse lr (j+1,t))
      in lr (0,t)
      end

  fun all (p : elem -> bool) (t:table) : bool =
      let val stop = length t
	  fun lr (j,t) = j >= stop orelse (p (tsub(t,j)) andalso lr (j+1,t))
      in lr (0,t)
      end

  fun collate cmp (t1: table, t2: table) =
      let val n1 = length t1
	  val n2 = length t2
	  val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point t1[0..j-1] = t2[0..j-1] *)
	      if j = stop then if      n1 < n2 then LESS
                               else if n1 > n2 then GREATER
                               else                 EQUAL
	      else case cmp(tsub(t1,j), tsub(t2,j)) of
		       EQUAL => h (j+1)
		     | res   => res
      in h 0
      end
end


(* ---------------------------------------------------------------
   WordSlice functor implementing monomorphic WordNVectorSlice and
   WordNArraySlice structures.
   --------------------------------------------------------------- *)

functor WordSlice (Arg : WORD_TABLE_ARG) =
struct
    open Arg

    val length_vector = vlen

    val maxLen = Initial.wordtable_maxlen

    type slice = table * int * int
    type vector_slice = vector * int * int

    fun length (a, i, n) = n

    fun sub ((a', i', n'), i) =
        if i<0 orelse i >= n' then raise Subscript
        else tsub(a',i'+i)

    fun update ((a', i', n'), i, v) =
        if i<0 orelse i>=n' then raise Subscript
        else tupd (a',i'+i,v);

    fun slice (a, i, len) =
        let val alen = tlen a
        in case len of
               NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
                         else raise Subscript
             | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
                         else raise Subscript
        end

    fun full a = (a, 0, tlen a);

    fun subslice ((a, i, n), i', NONE) =
        if 0<=i' andalso i'<=n then (a, i+i', n-i')
        else raise Subscript
      | subslice ((a, i, n), i', SOME n') =
        if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
        else raise Subscript

    fun base sli = sli

    fun vector (a : table, i, n) : vector =
        let val newvec = valloc n
            fun copy j =
                if j<n then (vupd(newvec,j,tsub(a,i+j)); copy (j+1))
                else ()
        in copy 0; newvec
        end

    fun copy {src=(a1,i1,n) : slice, dst=a2: table, di=i2} =
        if i2<0 orelse i2+n > tlen a2 then raise Subscript
        else if i1 < i2 then            (* copy from high to low *)
          let fun hi2lo j =
                  if j >= 0 then (tupd(a2,i2+j,tsub(a1,i1+j));
                                  hi2lo (j-1))
                  else ()
          in hi2lo (n-1)
          end
        else                       (* i1 >= i2, copy from low to high *)
          let fun lo2hi j =
                  if j < n then (tupd(a2,i2+j,tsub(a1,i1+j));
                                 lo2hi (j+1))
                  else ()
          in lo2hi 0
          end

    fun copyVec {src : vector_slice, dst=a2: table, di=i2} =
      let val (a1, i1, n) = src
      in if i2<0 orelse i2+n > tlen a2 then raise Subscript
         else let fun lo2hi j = if j < n then
                                  (tupd(a2,i2+j,vsub(a1,i1+j));
                                   lo2hi (j+1))
                                else ()
              in lo2hi 0
              end
      end

    fun isEmpty (_, _, n) = n=0

    fun concat slis : vector =
        let fun acc [] len                 = len
              | acc ((_, _, len1)::vr) len = acc vr (len1 + len)
            val len = acc slis 0
            val newvec = if len > maxLen then raise Size
                         else valloc len
            fun copyall to []                   = () (* Now: to = len *)
              | copyall to ((v1, i1, n1)::slir) =
                let fun copyv1 j =
                        if j<n1 then (vupd(newvec,to+j,tsub(v1,i1+j));
                                      copyv1 (j+1))
                        else ()
                in (copyv1 0; copyall (to+n1) slir)
                end
        in copyall 0 slis;
           newvec
        end

    fun getItem (a, i, 0) = NONE
      | getItem (a, i, n) = SOME(tsub(a,i), (a, i+1, n-1));

    fun find (p : elem -> bool) ((a,i,n) : slice) : elem option =
        let val stop = i+n
            fun lr j =
                if j < stop then
                  if p (tsub(a,j)) then SOME (tsub(a,j)) else lr (j+1)
                else NONE
        in lr i
        end

    fun exists (p : elem -> bool) ((a,i,n) : slice) : bool =
        let val stop = i+n
            fun lr j = j < stop andalso (p (tsub(a,j)) orelse lr (j+1))
        in lr i
        end

    fun all (p : elem -> bool) ((a,i,n) : slice) : bool =
        let val stop = i+n
            fun lr j = j >= stop orelse (p (tsub(a,j)) andalso lr (j+1))
        in lr i
        end

    fun app f (a, i, n) =
        let val stop = i+n
            fun lr j = if j < stop then (f(tsub(a,j)): unit; lr (j+1))
                       else ()
        in lr i
        end

    fun map (f : elem -> elem) (a : table, i, n) : table =
        let val newvec = talloc n
            val stop = i+n
            fun lr j =
                if j < stop then (tupd(newvec,j-i,f(tsub(a,j)));
                                  lr (j+1))
                else ()
        in lr i; newvec
        end

    fun foldl f e (a, i, n) =
        let val stop = i+n
            fun lr j res = if j < stop then lr (j+1) (f(tsub(a,j), res))
                           else res
        in lr i e
        end

    fun foldr f e (a, i, n) =
        let fun rl j res = if j >= i then rl (j-1) (f(tsub(a,j), res))
                           else res
        in rl (i+n-1) e
        end

    fun modify f (a, i, n) =
        let val stop = i+n
            fun lr j = if j < stop then (tupd(a,j,f(tsub(a,j))); lr (j+1))
                       else ()
        in lr i
        end

    fun findi (p : int * elem -> bool) ((a,i,n) : slice) : (int * elem) option =
        let val stop = i+n
            fun lr j =
                if j < stop then
                  if p (j-i, tsub(a,j)) then SOME (j-i, tsub(a,j)) else lr (j+1)
                else NONE
        in lr i
        end

    fun appi f (a, i, n) =
        let val stop = i+n
            fun lr j =
                if j < stop then (f(j-i, tsub(a,j)):unit; lr (j+1))
                else ()
        in lr i
        end

    fun mapi (f : int * elem -> elem) (a : table, i, n) : table =
        let val newvec = talloc n
            val stop = i+n
            fun lr j =
                if j < stop then (tupd(newvec,j-i,f(j-i, tsub(a,j)));
                                  lr (j+1))
                else ()
        in lr i; newvec
        end

    fun foldli f e (a, i, n) =
        let val stop = i+n
            fun lr j res =
                if j < stop then lr (j+1) (f(j-i, tsub(a,j), res))
                else res
        in lr i e
        end

    fun foldri f e (a, i, n) =
        let fun rl j res =
                if j >= i then rl (j-1) (f(j-i, tsub(a,j), res))
                else res
        in rl (i+n-1) e
        end

    fun modifyi f (a, i, n) =
        let val stop = i+n
            fun lr j =
                if j < stop then (tupd(a,j,f(j-i, tsub(a,j))); lr (j+1))
                else ()
        in lr i
        end

    fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
        let val stop = if n1 < n2 then n1 else n2
            fun h j = (* At this point a1[i1..i1+j-1] = a2[i2..i2+j-1] *)
                if j = stop then if      n1 < n2 then LESS
                                 else if n1 > n2 then GREATER
                                 else                 EQUAL
                else
                  case cmp(tsub(a1,i1+j), tsub(a2,i2+j)) of
                      EQUAL => h (j+1)
                    | res   => res
        in h 0
        end
end

(* ---------------------------------------------------------------
   WordArray2 functor implementing monomorphic 2-dimensional arrays.
   --------------------------------------------------------------- *)

functor WordArray2 (Arg: WORD_TABLE_ARG where type table = chararray)
        : MONO_ARRAY2 where type elem = Arg.elem =
struct

open Arg

val maxLen : int = Initial.wordtable_maxlen

fun check_size r c =
    if 0 <= r andalso 0 <= c then
      let val n = (c*r) handle Overflow => raise Size
      in if n <= maxLen then n
         else raise Size
      end
    else raise Size

fun alloc_table r c =
    talloc (check_size r c)

fun tabulatev (n, f : int -> elem) : vector =
    let fun init f (t, i) = if i >= n then t
			    else (vupd (t, i, f i); init f (t, i+1))
    in init f (valloc n, 0)
    end

type array = {base: table,
              ncols: int,
              nrows: int}

type region = {base  : array,
               row   : int,
               col   : int,
               nrows : int option,
               ncols : int option}

fun dimensions ({nrows,ncols,...}: array) : int * int =
    (nrows,ncols)

fun nCols (a : array) : int = #2 (dimensions a)
fun nRows (a : array) : int = #1 (dimensions a)

fun check (a,r,c) : unit =
    if r < 0 orelse c < 0 orelse r >= nRows a orelse c >= nCols a then raise Subscript
    else ()

fun check_region ({base,row,col,nrows,ncols}:region) : unit =
    if row < 0 orelse col < 0 then raise Subscript
    else if row > nRows base orelse col > nCols base then raise Subscript
    else ((case nrows of
               NONE => ()
             | SOME n => if n < 0 orelse n+row > nRows base then raise Subscript
                         else ())
         ; (case ncols of
                NONE => ()
              | SOME n => if n < 0 orelse n+col > nCols base then raise Subscript
                          else ()))

datatype traversal = datatype Array2.traversal

fun iter n f =
    let fun loop i =
            if i >= n then ()
            else (f i ; loop (i+1))
    in loop 0
    end

fun array (nrows,ncols,e) : array =
    let val a = alloc_table nrows ncols
    in iter (tlen a) (fn i => tupd(a,i,e))
     ; {base = a,
        nrows = nrows,
        ncols = ncols}
    end

fun fromList (m: elem list list) : array =
    let val nrows = List.length m
        val ncols = case m of
                        r::rs =>
                        let val k = List.length r
                            val ns = List.map List.length rs
                        in if List.all (fn n => k = n) ns then k
                           else raise Size
                        end
                      | nil => 0
        fun updr (a,i,es,j) =
            case es of
                nil => ()
              | e::es => ( tupd(a,i*ncols+j,e)
                         ; updr (a,i,es,j+1)
                         )
        fun upd (a,rs,i) =
            case rs of
                nil => ()
              | r::rs => ( updr (a,i,r,0)
                         ; upd (a,rs,i+1)
                         )
        val a = alloc_table nrows ncols
    in upd (a,m,0)
     ; {nrows=nrows,ncols=ncols,base=a}
    end

fun trav t (f:int*int*'a->'a) ({base,col,row,nrows,ncols}:region) (acc:'a) : 'a =
    let val nrows = case nrows of
                        SOME h => row + h
                      | NONE => #nrows base
        val ncols = case ncols of
                        SOME w => col + w
                      | NONE => #ncols base
        fun loop_rm (acc,i,j) = if i >= nrows then acc
                                else if j >= ncols then
                                  loop_rm(acc,i+1,col)
                                else loop_rm(f(i,j,acc),i,j+1)
        fun loop_cm (acc,i,j) = if j >= ncols then acc
                                else if i >= nrows then
                                  loop_cm(acc,row,j+1)
                                else (loop_cm(f(i,j,acc),i+1,j))
    in case t of
           RowMajor => loop_rm (acc,row,col)
         | ColMajor => loop_cm (acc,row,col)
    end

fun region (a:array) : region = {base=a,row=0,col=0,nrows=NONE,ncols=NONE}

fun sub_unsafe ({nrows,ncols,base}: array, i, j) : elem =
    tsub(base,i*ncols+j)

fun sub (a:array, r, c) : elem =
    ( check (a,r,c)
    ; sub_unsafe(a,r,c))

fun update_unsafe ({nrows,ncols,base}: array, i, j, e) : unit =
    tupd(base,i*ncols+j,e)

fun update (a,i,j,e) =
    ( check (a,i,j)
    ; update_unsafe(a,i,j,e))

fun tabulate (t: traversal) (nrows,ncols,f:int*int->elem) : array =
    let val base = alloc_table nrows ncols
        val arr : array = {base=base,nrows=nrows,ncols=ncols}
        fun g (i,j,()) = update_unsafe(arr,i,j,f(i,j))
    in trav t g (region arr) ()
     ; arr
    end

fun row (a:array,r) : vector =
    if r < 0 orelse r >= nRows a then raise Subscript
    else tabulatev(nCols a,fn c => sub_unsafe(a,r,c))

fun column (a:array,c) : vector =
    if c < 0 orelse c >= nCols a then raise Subscript
    else tabulatev(nRows a,fn r => sub_unsafe(a,r,c))

fun appi (t:traversal) (f: int*int*elem->unit) (r:region) : unit =
    ( check_region r
    ; trav t (fn (i,j,()) => f(i,j,sub_unsafe(#base r,i,j))) r ()
    )

local
fun traverseInit ({base,row,col,nrows,ncols}: region)
    : {nR:int,nC:int,rstop:int,cstop:int} =
    let val () = if row < 0 orelse col < 0 then raise Subscript
                 else ()
        val (nR,nC) = dimensions base
        val rstop = case nrows of
                        SOME nr =>
                        let val rstop = row+nr
                        in if rstop > nR then raise Subscript
                           else rstop
                        end
                      | NONE => nR
        val cstop = case ncols of
                        SOME nc =>
                        let val cstop = col+nc
                        in if cstop > nC then raise Subscript
                           else cstop
                        end
                      | NONE => nC
    in {nR=nR,nC=nC,rstop=rstop,cstop=cstop}
    end
in
fun copy {src : region,
          dst : array,
          dst_row : int,
          dst_col : int} : unit =
    let val {nR:int,nC:int,rstop:int,cstop:int} = traverseInit src
        val r_reg = rstop - (#row src)
        val c_reg = cstop - (#col src)
        val () = if dst_row < 0 orelse dst_row + r_reg > nR orelse
                    dst_col < 0 orelse dst_col + c_reg > nC
                 then raise Subscript
                 else ()
        val tmp = tabulate RowMajor
                           (r_reg, c_reg,
                            fn(r,c) => sub_unsafe(#base src,
                                                  r + #row src,
                                                  c + #col src))
    in appi RowMajor (fn (r,c,v) =>
                         update_unsafe(dst,dst_row+r,dst_col+c,v))
            {base=tmp,row=0,col=0,nrows=NONE,ncols=NONE}
    end

end

fun app (t:traversal) (f:elem->unit) (a:array) : unit =
    trav t (fn (i,j,()) => f (sub_unsafe(a,i,j))) (region a) ()

fun foldi (t:traversal) (f:int*int*elem*'b->'b) (acc:'b) (r:region) : 'b =
    ( check_region r
    ; trav t (fn (i,j,acc) => f(i,j,sub_unsafe(#base r,i,j),acc)) r acc
    )

fun fold (t:traversal) (f:elem*'b->'b) (acc:'b) (a:array) : 'b =
    trav t (fn (i,j,acc) => f(sub_unsafe(a,i,j),acc)) (region a) acc

fun modifyi (t:traversal) (f:int*int*elem->elem) (r:region) : unit =
    ( check_region r
    ; trav t (fn (i,j,()) => update_unsafe(#base r,i,j,f(i,j,sub_unsafe(#base r,i,j)))) r ()
    )

fun modify (t:traversal) (f:elem->elem) (a:array) : unit =
    trav t (fn (i,j,()) => update_unsafe(a,i,j,f(sub_unsafe(a,i,j)))) (region a) ()

end

(* ---------------------------------------------------------------
   Functor for building and collecting table structures with the
   proper type abstractions in place.
   --------------------------------------------------------------- *)

functor BuildTables(structure ArgV : WORD_TABLE_ARG where type table = string
                    structure ArgA : WORD_TABLE_ARG where type table = chararray
                    sharing type ArgV.elem = ArgA.elem)
        :> TABLES where type V.elem = ArgV.elem =
struct
  structure V : MONO_VECTOR = struct
    structure V = WordTable(ArgV)
    open V val update = updatev
  end
  structure VS : MONO_VECTOR_SLICE = WordSlice(ArgV)
  structure A : MONO_ARRAY = WordTable(ArgA)
  structure AS : MONO_ARRAY_SLICE = WordSlice(ArgA)
  structure A2 : MONO_ARRAY2 = WordArray2(ArgA)
end

(* ---------------------------------------------------------------
   Here comes the basic functors for building the argument structures
   to the BuildTables functor. There is one functor for each
   underlying type (e.g., word31, bool, real, ...) The functors are
   applied in the file wordtables.sml.
   --------------------------------------------------------------- *)

functor TableArgBool(type table) : WORD_TABLE_ARG = struct
  type elem = bool
  fun w2b (w:Word8.word) : bool = (w = 0w1)
  fun b2w (b:bool) : Word8.word = if b then 0w1 else 0w0
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  fun vsub (t:vector,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  type table = table
  fun tsub (t:table,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgWord8(type table) : WORD_TABLE_ARG = struct
  type elem = Word8.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgWord16(type table) : WORD_TABLE_ARG = struct
  type elem = Word16.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 2
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w1))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w1))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord31(type table) : WORD_TABLE_ARG = struct
  type elem = Word31.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord32(type table) : WORD_TABLE_ARG = struct
  type elem = Word32.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord63(type table) : WORD_TABLE_ARG = struct
  type elem = Word63.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord64(type table) = struct
  type elem = Word64.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord(type table) : WORD_TABLE_ARG = struct
  type elem = Word.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgReal(type table) : WORD_TABLE_ARG = struct
  type elem = real
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  fun tlen (t:table) : int = prim ("__blockf64_size", t)
  fun vlen (t:vector) : int = prim ("__blockf64_size", t)
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt8(type table) : WORD_TABLE_ARG = struct
  type elem = Int8.int
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgInt16(type table) : WORD_TABLE_ARG = struct
  type elem = Int16.int
  type array = chararray
  type vector = string
  val wordSizeBytes = 2
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w1))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w1))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt31(type table) : WORD_TABLE_ARG = struct
  type elem = int31
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt32(type table) : WORD_TABLE_ARG = struct
  type elem = int32
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt63(type table) : WORD_TABLE_ARG = struct
  type elem = int63
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt64(type table) = struct
  type elem = int64
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt(type table) : WORD_TABLE_ARG = struct
  type elem = int
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.<<(Word.fromInt n, 0w3))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end
