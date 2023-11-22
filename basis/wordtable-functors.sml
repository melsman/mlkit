(* ---------------------------------------------------------------
   Functors for implementing unboxed monomorphic arrays, vectors,
   and slices with word values as the underlying element type.
   --------------------------------------------------------------- *)

signature WORD_TABLE_ARG = sig
  type elem
  type table
  type vector = string
  type array = chararray
  val tsub          : table * int -> elem
  val tupd          : table * int * elem -> unit
  val vsub          : vector * int -> elem
  val vupd          : vector * int * elem -> unit
  val asub          : array * int -> elem
  val aupd          : array * int * elem -> unit
  val maxLen        : int
  val wordSizeBytes : int
end

(* ---------------------------------------------------------------
   WordTable functor implementing monomorphic WordNVector and
   WordNArray structures.

   Internally, vectors are represented as strings and arrays as
   chararrays, which allows for the proper equality tests.

   The type table is the basic table type with functions
     sub0    : extract element from table
     update0 : update element in table
     table0  : construct new table
   The table type is defined in Runtime/Table.h.
 ----------------------------------------------------------------- *)

functor WordTable (Arg : WORD_TABLE_ARG) =
struct
  open Arg

  fun table0 (n:int) : table =
      prim("allocStringML", n * wordSizeBytes)

  fun vector0 (n:int) : vector =
      prim("allocStringML", n * wordSizeBytes)

  fun array0 (n:int) : array =
      prim("allocStringML", n * wordSizeBytes)

  fun length (t:table) : int =
      prim ("__bytetable_size", t) div wordSizeBytes

  fun length_vector (t:vector) : int =
      prim ("__bytetable_size", t) div wordSizeBytes

  (* quite a few bits are available for the length in the tag field! *)
  val maxLen = maxLen

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
      (chk_sz n; table0 n)

  (* vector n returns an uninitialised vector with n elements.
   * Raise Size if n > maxLen. *)
  fun vectorv n =
      (chk_sz n; vector0 n)

  (* array n gives an initialised array with n elements. *)
  (* Raise Size if n > maxLen.                           *)
  fun array (n, x) =
      let val () = chk_sz n
          val a = array0 n
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
      in init f (vector0 n, 0)
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
	  val newvec = table0 stop
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
          val b : table = table0 n
	  fun lr j =
	      if j < n then (tupd (b, j, f (tsub (a, j))); lr (j+1))
	      else ()
      in lr 0; b
    end

  fun mapi (f : int * elem -> elem) (a:table) : table =
      let val stop = length a
	  val newtab = table0 stop
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

    fun table0 (i:int) : table =
        prim("allocStringML", i * wordSizeBytes)

    fun vector0 (i:int) : vector =
        prim("allocStringML", i * wordSizeBytes)

    fun length0 (t:table): int =
        prim ("__bytetable_size", t) div wordSizeBytes

    fun length_vector (v:vector): int =
        prim ("__bytetable_size", v) div wordSizeBytes

(*    fun null () : elem = prim("id",0:int) *)

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
        let val alen = length0 a
        in case len of
               NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
                         else raise Subscript
             | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
                         else raise Subscript
        end

    fun full a = (a, 0, length0 a);

    fun subslice ((a, i, n), i', NONE) =
        if 0<=i' andalso i'<=n then (a, i+i', n-i')
        else raise Subscript
      | subslice ((a, i, n), i', SOME n') =
        if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
        else raise Subscript

    fun base sli = sli

    fun vector (a : table, i, n) : vector =
        let val newvec = vector0 n
            fun copy j =
                if j<n then (vupd(newvec,j,tsub(a,i+j)); copy (j+1))
                else () (*vupd(newvec,j,null())*)
        in copy 0; newvec
        end

    fun copy {src=(a1,i1,n) : slice, dst=a2: table, di=i2} =
        if i2<0 orelse i2+n > length0 a2 then raise Subscript
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
      in if i2<0 orelse i2+n > length0 a2 then raise Subscript
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
                         else vector0 len
            fun copyall to []                   = () (* Now: to = len *)
              | copyall to ((v1, i1, n1)::slir) =
                let fun copyv1 j =
                        if j<n1 then (vupd(newvec,to+j,tsub(v1,i1+j));
                                      copyv1 (j+1))
                        else ()
                in (copyv1 0; copyall (to+n1) slir)
                end
        in copyall 0 slis;
           (* vupd(newvec,len,null()); *)
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
        let val newvec = table0 n
            val stop = i+n
            fun lr j =
                if j < stop then (tupd(newvec,j-i,f(tsub(a,j)));
                                  lr (j+1))
                else () (*tupd(newvec,j-i,null())*)
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
        let val newvec = table0 n
            val stop = i+n
            fun lr j =
                if j < stop then (tupd(newvec,j-i,f(j-i, tsub(a,j)));
                                  lr (j+1))
                else () (*tupd(newvec,j-i,null())*)
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

functor TableArgBool(type table) : WORD_TABLE_ARG = struct
  type elem = bool
  fun w2b (w:Word8.word) : bool = (w = 0w1)
  fun b2w (b:bool) : Word8.word = if b then 0w1 else 0w0
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem =
      w2b(prim ("__bytetable_sub", (t,i)))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,b2w e))
  fun vsub (t:vector,i:int) : elem =
      w2b(prim ("__bytetable_sub", (t,i)))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,b2w e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      w2b(prim ("__bytetable_sub", (t,i)))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,b2w e))
end

functor TableArgWord8(type table) : WORD_TABLE_ARG = struct
  type elem = Word8.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update", (t,i,e))
end

functor TableArgWord16(type table) : WORD_TABLE_ARG = struct
  type elem = Word16.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 2
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word16", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word16", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word16", (t,i,e))
end

functor TableArgWord31(type table) : WORD_TABLE_ARG = struct
  type elem = Word31.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 4
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word31", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word31", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word31", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word31", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word31", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word31", (t,i,e))
end

functor TableArgWord32(type table) : WORD_TABLE_ARG = struct
  type elem = Word32.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 4
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word32", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word32", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word32", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word32", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word32", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word32", (t,i,e))
end

functor TableArgWord63(type table) : WORD_TABLE_ARG = struct
  type elem = Word63.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 8
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word63", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word63", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word63", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word63", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word63", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word63", (t,i,e))
end

functor TableArgWord64(type table) = struct
  type elem = Word64.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 8
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word64", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word64", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word64", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word64", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word64", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word64", (t,i,e))
end

functor TableArgWord(type table) : WORD_TABLE_ARG = struct
  type elem = Word.word
  type array = chararray
  type vector = string
  val maxLen = 1000000000
  val wordSizeBytes = 8
  fun asub (t:array,i:int) : elem =
      prim ("__bytetable_sub_word", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit =
      prim("__bytetable_update_word", (t,i,e))
  fun vsub (t:vector,i:int) : elem =
      prim ("__bytetable_sub_word", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit =
      prim("__bytetable_update_word", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem =
      prim ("__bytetable_sub_word", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit =
      prim("__bytetable_update_word", (t,i,e))
end
