functor RealArray2 (Arg: WORD_TABLE_ARG where type table = chararray)
        : MONO_ARRAY2 where type elem = Arg.elem =
struct

open Arg

fun vector0 (n:int) : vector =
    prim("allocStringML", n * wordSizeBytes)

fun alloc (n:int) : table =
    prim("allocStringML", n * wordSizeBytes)

fun length (t:table) : int =
    prim ("__blockf64_size", t)

val maxLen : int = Initial.wordtable_maxlen

fun check_size r c =
    if 0 <= r andalso 0 <= c then
      let val n = (c*r) handle Overflow => raise Size
      in if n <= maxLen then n
         else raise Size
      end
    else raise Size

fun alloc_table r c =
    alloc (check_size r c)

fun tabulatev (n, f : int -> elem) : vector =
    let fun init f (t, i) = if i >= n then t
			    else (vupd (t, i, f i); init f (t, i+1))
    in init f (vector0 n, 0)
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
    in iter (length a) (fn i => tupd(a,i,e))
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
