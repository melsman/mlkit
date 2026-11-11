(* Explicit real operations expose boxing independently of Basis inlining. *)
val () =
  let
    exception FloatFlatteningFailure
    infix 6 -
    infix 4 =
    fun op = (x:''a,y:''a) : bool = prim("=",(x,y))
    fun add (x:real,y:real) : real = prim("__plus_real", (x,y))
    fun equal (x:real,y:real) : bool = prim("__lesseq_real", (x,y)) andalso prim("__lesseq_real", (y,x))
    fun deref (r:'a ref) : 'a = prim("!", r)
    fun assign (r:'a ref,x:'a) : unit = prim(":=", (r,x))
    val input = ref 1
    fun real (x:int) : real = prim("realInt",x)
    val effects = ref ""
    fun mark s = assign(effects,s)
    fun inspect n (p:string * real * real * int) =
          if n = 0 then (add(#2 p,#3 p),#1 p,#4 p)
          else inspect (n-1) p
    val p = ((mark "first"; "before"),
             add((mark "float"; real(deref input)),2.0),
             add((mark "last"; real(deref input)),2.0),7)
    val result = inspect 10 p
    datatype sample = Sample of string * real
    fun sampleValue n (Sample(_,x)) =
        if n = 0 then add(x,1.0) else sampleValue (n-1) (Sample("copy",add(x,0.0)))
    val sampleResult = sampleValue 10 (Sample("original",add(real(deref input),2.0)))
    fun mutableValue n (p:real ref * string) =
        if n = 0 then add(deref(#1 p),0.0) else mutableValue (n-1) p
    val cell = (ref(add(real(deref input),2.0)),"cell")
    val () = assign(#1 cell,add(real(deref input),3.0))
    val mutableResult = mutableValue 10 cell
    (* Literal and computed initializers must agree on one layout. Check that
       flattening preserves negative zero and permits literal stores. *)
    fun divide (x:real,y:real) : real = prim("__div_real", (x,y))
    fun literalValue__noinline (p:string * real) = divide(1.0,#2 p)
    val negative = literalValue__noinline ("negative",~0.0)
    val positive = literalValue__noinline ("positive",0.0)
    val computed = literalValue__noinline ("computed",add(real(deref input),1.0))
    val () = assign(#1 cell,~0.0)
    val stored = divide(1.0,deref(#1 cell))
    val literalsOK = prim("__less_real",(negative,0.0)) andalso
                     prim("__less_real",(0.0,positive)) andalso
                     prim("__less_real",(stored,0.0)) andalso equal(computed,0.5)
    val orderOK = deref effects = "last"
    exception Stop
    fun check__noinline (q:string * real * int) = equal(#2 q,3.0)
    val caught =
        (check__noinline
          ((mark "before exception"; "before"),
           add((if deref input = 1 then raise Stop else real(deref input)),2.0),
           (mark "after exception"; 7)); false)
        handle Stop => true
  in
    if equal(sampleResult,4.0) andalso equal(mutableResult,4.0) andalso
       equal(#1 result,6.0) andalso #2 result = "before" andalso
       #3 result = 7 andalso literalsOK andalso orderOK andalso caught andalso deref effects = "before exception"
    then print "float data flattening passed\n"
    else raise FloatFlatteningFailure
  end
