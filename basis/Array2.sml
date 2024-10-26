(* MIT License. Copyright (c) 2020 Martin Elsman *)

structure Array2 :> ARRAY2 = struct

type 'a array = 'a array

(* quite a few bits are available for the length in the tag field! *)
val maxLen = Initial.wordtable_maxlen

type 'a region = {base  : 'a array,
                  row   : int,
                  col   : int,
                  nrows : int option,
                  ncols : int option}

datatype traversal = RowMajor | ColMajor

fun nRows (a : 'a array) : int = prim ("word_sub0", (a, 0))
fun nCols (a : 'a array) : int = prim ("word_sub0", (a, 1))
fun dimensions (a: 'a array) : int * int = (nRows a, nCols a)
fun set_rows (a : 'a array,r:int) : unit = prim ("word_update0", (a, 0, r))
fun set_cols (a : 'a array,c:int) : unit = prim ("word_update0", (a, 1, c))

fun sub2 (a : 'a array, cols:int, r:int, c:int) : 'a =
    prim ("word_sub0", (a, r*cols+c+2))

fun update2 (a : 'a array, cols:int, r:int, c:int, v:'a) : unit =
    prim ("word_update0", (a, r*cols+c+2, v))

(* The primitive word_table2d0 is in OptLambda compiled into calls to
   word_table0 and consecutive updates to store the sizes of each
   dimension in slot 0 and 1. Similarly for word_table2d0_init, which
   is in OptLambda compiles into calls to word_table_init and
   consecutive updates to store the sizes of each dimension in slot 0
   and 1.

   It is safe (also when gc is enabled) to store integers in the first
   slots of arrays allocated with word_table0 as the integers are
   tagged when gc is enabled.
*)

fun table2d0 (n:int,r:int,c:int) : 'a array = prim ("word_table2d0", (n,r,c))
fun table2d0_init (n:int,v:'a,r:int,c:int) : 'a array = prim ("word_table2d0_init", (n,v,r,c))

fun update0 (a : 'a array, i : int, x : 'a) : unit = prim ("word_update0", (a, i, x))

fun check (nr,nc) : int =
    if nr < 0 orelse nc < 0 then raise Size
    else let val n = nr*nc handle Overflow => raise Size
         in if n > maxLen then raise Size
            else n
         end

fun array (nr:int,nc:int,v:'a):'a array =
    let val n = check(nr,nc)
        val a = table2d0_init(n+2,v,nr,nc)
    in a
    end

fun sub (a: 'a array, r:int, c:int) : 'a =
    let val (rs,cs) = dimensions a
    in if r < 0 orelse c < 0 orelse r >= rs orelse
          c >= cs then raise Subscript
       else sub2 (a,cs,r,c)
    end

fun update (a: 'a array, r:int, c:int, v:'a) : unit =
    let val (rs,cs) = dimensions a
    in if r < 0 orelse c < 0 orelse r >= rs orelse
          c >= cs then raise Subscript
       else update2 (a,cs,r,c,v)
    end

fun fromList (rs : 'a list list) : 'a array =
    let val nr = List.length rs
        val nc = case rs of
                     c :: _ => List.length c
                   | nil => 0
        val () = List.app (fn r => if List.length r <> nc then raise Size
                                   else ()) rs
        val a = table2d0 (nr*nc+2,nr,nc)
    in foldl(fn (r,i) =>
                foldl(fn (x,i) => (update0(a,i,x); i+1)) i r) 2 rs
     ; a
    end


fun tabulate t (nr:int,nc:int,f:int*int-> 'a) : 'a array =
    let val n = check(nr,nc)
        val a = table2d0(n+2,nr,nc)
    in case t of
           RowMajor =>
           let fun loopC (r,c) = if c >= nc then a
                                 else (update2(a,nc,r,c,f(r,c)); loopC (r,c+1))
               fun loopR (r,c) = if r >= nr then a
                                 else (loopC(r,c); loopR(r+1,c))
           in loopR (0,0)
           end
         | ColMajor =>
           let fun loopR (r,c) = if r >= nr then a
                                 else (update2(a,nc,r,c,f(r,c)); loopR (r+1,c))
               fun loopC (r,c) = if c >= nc then a
                                 else (loopR(r,c); loopC(r,c+1))
           in loopC (0,0)
           end
    end

fun traverseInit ({base,row,col,nrows,ncols}: 'a region)
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

fun traverseRM (f: int*int*'a*'b -> 'b) (a:'b)
               (reg as {base,row,col,nrows,ncols}: 'a region) : 'b =
    let val {nR,nC,rstop,cstop} = traverseInit reg
        fun loopC (r,c,a,reg: 'a region) = if c >= cstop then a
                                           else loopC (r,c+1,f(r,c,sub2(base,nC,r,c),a),reg)
        fun loopR (r,c,a,reg: 'a region) = if r >= rstop then a
                                           else loopR(r+1,c,loopC(r,c,a,reg),reg)
    in loopR (row,col,a,reg)
    end

fun traverseCM (f: int*int*'a*'b -> 'b) (a:'b)
               (reg as {base,row,col,nrows,ncols}: 'a region) : 'b =
    let val {nR,nC,rstop,cstop} = traverseInit reg
        fun loopR (r,c,a,reg: 'a region) = if r >= rstop then a
                                           else loopR (r+1,c,f(r,c,sub2(base,nC,r,c),a),reg)
        fun loopC (r,c,a,reg: 'a region) = if c >= cstop then a
                                           else loopC(r,c+1,loopR(r,c,a,reg),reg)
    in loopC (row,col,a,reg)
    end

fun traverse (t:traversal) (f: int*int*'a*'b -> 'b)
             (a:'b) (reg:'a region) : 'b =
    case t of
        RowMajor => traverseRM f a reg
      | ColMajor => traverseCM f a reg

fun mkRegion (a: 'a array) : 'a region =
   {base=a,row=0,col=0,nrows=NONE,ncols=NONE}

fun appi (t:traversal) (f: int*int*'a -> unit) (reg: 'a region) : unit =
    traverse t (fn (r,c,v,()) => f(r,c,v)) () reg

fun app (t:traversal) (f: 'a -> unit) (arr: 'a array) : unit =
    traverse t (fn (_,_,v,()) => f v) () (mkRegion arr)

fun foldi (t:traversal) (f: int*int*'a*'b -> 'b) (a: 'b) (reg: 'a region) : 'b =
    traverse t (fn (r,c,v,a) => f(r,c,v,a)) a reg

fun fold (t:traversal) (f: 'a*'b -> 'b) (a: 'b) (arr: 'a array) : 'b =
    traverse t (fn (_,_,v,a) => f(v,a)) a (mkRegion arr)

fun modifyi (t:traversal) (f: int*int*'a -> 'a) (reg: 'a region) : unit =
    let val nC = nCols(#base reg)
    in traverse t (fn (r,c,v,()) => update2(#base reg,nC,r,c,f(r,c,v))) () reg
    end

fun modify (t:traversal) (f: 'a -> 'a) (arr: 'a array) : unit =
    modifyi t (fn (_,_,v) => f v) (mkRegion arr)

fun row (a:'a array, r:int) : 'a Vector.vector =
    let val (rs,cs) = dimensions a
    in if r < 0 orelse r >= rs then raise Subscript
       else Vector.tabulate (cs, fn c => sub2(a,cs,r,c))
    end

fun column (a:'a array, c:int) : 'a Vector.vector =
    let val (rs,cs) = dimensions a
    in if c < 0 orelse c >= cs then raise Subscript
       else Vector.tabulate (rs, fn r => sub2(a,cs,r,c))
    end

fun copy {src: 'a region, dst: 'a array, dst_row:int, dst_col:int} : unit =
    let val {nR:int,nC:int,rstop:int,cstop:int} = traverseInit src
        val r_reg = rstop - (#row src)
        val c_reg = cstop - (#col src)
        val () = if dst_row < 0 orelse dst_row + r_reg > nR orelse
                    dst_col < 0 orelse dst_col + c_reg > nC
                 then raise Subscript
                 else ()
        val tmp = tabulate RowMajor
                           (r_reg, c_reg,
                            fn(r,c) => sub2(#base src,nC,
                                            r + #row src,
                                            c + #col src))
    in appi RowMajor (fn (r,c,v) =>
                         update2(dst,nC,dst_row+r,dst_col+c,v))
            {base=tmp,row=0,col=0,nrows=NONE,ncols=NONE}
    end

end
