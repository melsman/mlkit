(* Conditional real results can reuse an operand instead of allocating a box. *)
val () =
  let
    infix 7 /
    infix 6 +
    infix 4 =
    fun op = (x:''a,y:''a) : bool = prim("=",(x,y))
    fun deref (x:'a ref) : 'a = prim("!",x)
    fun assign (x:'a ref,y:'a) : unit = prim(":=",(x,y))
    exception ConditionalFloatFailure
    fun not b = if b then false else true
    fun nan (x:real) : bool = prim("isnanFloat",x)
    fun add (x:real,y:real) : real = prim("__plus_real",(x,y))
    fun equal (x:real,y:real) = prim("__lesseq_real",(x,y)) andalso prim("__lesseq_real",(y,x))
    fun less (x:real,y:real) : bool = prim("__less_real",(x,y))
    (* A fresh boxed finite result, using portable primitives. Ray separately
       exercises the native min/max primitives with the same NaN branches. *)
    fun minimum (x:real,y:real) =
        if nan x then y else if nan y then x else add((if less(x,y) then x else y),~0.0)
    fun maximum (x:real,y:real) =
        if nan x then y else if nan y then x else add((if less(x,y) then y else x),~0.0)
    datatype bounds = Bounds of real * real * real
    fun merge__noinline (Bounds(x,y,z),Bounds(a,b,c)) =
        Bounds(minimum(x,a),maximum(y,b),minimum(z,c))
    fun check__noinline (Bounds(x,y,z)) =
        equal(x,2.0) andalso equal(y,7.0) andalso less(1.0 / z,0.0)
    val zero = ref 0.0
    val qnan = deref zero / deref zero
    val a = Bounds(4.0,7.0,~0.0)
    val b = Bounds(2.0,deref zero / deref zero,deref zero / deref zero)
    val result = merge__noinline(a,b)
    val calls = ref 0
    fun next x = (assign(calls,deref calls + 1); x)
    val predicateOK = nan(next qnan) andalso not(nan(next (1.0 / deref zero))) andalso
                      not(nan(next (~1.0 / deref zero))) andalso not(nan(next ~0.0)) andalso
                      not(nan(next 1.0)) andalso deref calls = 5
    val order = ref ""
    fun choose x =
        if nan(next x) then (assign(order,"nan"); ~0.0)
        else (assign(order,"finite"); add(x,1.0))
    fun inspect__noinline (x:real,y:real,z:real) =
        less(1.0 / x,0.0) andalso equal(y,3.0) andalso equal(z,5.0)
    val ordered = (choose qnan,choose 2.0,add((assign(order,"last"); 3.0),2.0))
    val orderOK = inspect__noinline ordered andalso deref order = "last" andalso deref calls = 7
    exception Stop
    val skipped = (if nan qnan then ~0.0 else raise Stop,add(1.0,2.0),add(2.0,3.0))
    val skippedOK = inspect__noinline skipped
    val caught =
        (nan(next (raise Stop)); false) handle Stop => true
  in
    if check__noinline result andalso predicateOK andalso orderOK andalso skippedOK andalso caught andalso deref calls = 7
    then print "float data flattening passed\n"
    else raise ConditionalFloatFailure
  end
