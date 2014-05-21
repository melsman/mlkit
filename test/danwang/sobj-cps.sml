
structure SObjCPS :> SOBJ_CPS =
  struct

    datatype sval = 
	vNil | vT | vF | vI of int
      | vCons of sval ref * sval ref
      | vLambda of (sval ref) ->
	(((int * sval ref list) * (sval ref)) -> ((int * sval ref list) * (sval ref))) ->
	(((int * sval ref list) * (sval ref)) -> ((int * sval ref list) * (sval ref)))
      | vFwd of sval ref
     
    type sobj = sval ref
    type sheap = (int * sobj list)
    type scont = (sheap * sobj) -> (sheap * sobj)
    type sexp = scont -> scont

   val limit = ref 0
   fun fwd_atomic v ((c,h),r) = 
     let val obj = ref (!r)
     in r := (vFwd obj);
       ((c+1,obj::h),obj)
     end	
   fun	fwd (h,r as (ref vNil)) = fwd_atomic vNil (h,r)
     | fwd (h,r as (ref vT)) = fwd_atomic vT (h,r)
     | fwd (h,r as (ref vF)) = fwd_atomic vF (h,r)
     | fwd (h,r as (ref (vI i))) = fwd_atomic (vI i) (h,r)
     | fwd (h,r as (ref (vLambda f))) = fwd_atomic (vLambda f) (h,r)
     | fwd (h,r as (ref (vCons(o1,o2)))) =
     let
       val (h',o1') = fwd (h,o1)
       val (h'',o2') = fwd (h',o2)
     in fwd_atomic (vCons(o1',o2')) (h'',r)
     end
     | fwd (h,ref (vFwd obj)) = (h,obj)
   fun do_gc (h,obj) = fwd ((0,[]),obj)
   fun maybe_gc k ((c,h),obj) =
     (print "check limit\n";
      if c < (!limit) then k ((c,h),obj)
      else (print "gc inovked\n";k (fwd ((0,[]),obj))))
       
   fun alloc (v:sval) (k:scont) =
     (fn (((c,h),_):(sheap * sobj)) =>
	 let val obj = ref v
	 in k ((c+1,obj::h),obj)
	 end)

    fun	mBind f (e:sexp) (k:scont) = (e (fn v => (f v k)))
    fun mUnit v k  = k v

    val Nil:sexp = alloc vNil
    val T:sexp = alloc vT
    val F:sexp = alloc vF
    fun I i = alloc (vI i)

    fun Cons (se1:sexp,se2:sexp) k =
      (se1 (fn v1 =>
     ((se2 (fn v2 => (alloc (vCons(#2 v1,#2 v2)) k) v2)) v1)))

    fun V v (k:scont) (h,obj) = k (h,v)
      
    fun Lambda f = alloc (vLambda f)
    fun	If (se,se_t,se_f) = let
	  fun test (v as (h,ref vT)) k = ((se_t k) v)
	    | test (v as (h,ref vF)) k = ((se_f k) v)
            | test _ _ = raise (Fail "if expected bool")
	in mBind test se
	end

    fun Car se = let
	  fun f (h,ref (vCons (x,y))) k = k (h,x)
            | f _ k = raise (Fail "car expected pair")
	in mBind f se
	end

    fun Cdr se = let
	  fun f (h,ref (vCons (x,y))) k = k (h,y)
            | f _ k = raise (Fail "cdr expected pair")
	in mBind f se
	end

    fun	Apply (se1:sexp,se2:sexp) k = 
      (se1 (fn (v as (h,ref (vLambda sf))) =>
	     (se2 (fn arg => (((sf (#2 arg)) (maybe_gc k)) arg)) v)
            | _ => raise (Fail "apply expected lambda")))

    fun	nilPred vNil = T
      | nilPred  _ = F

    fun	boolPred vT = T
      | boolPred vF = T
      | boolPred  _ = F

    fun	intPred (vI i) = T
      | intPred _ = F

    fun	pairPred (vCons _) = T
      | pairPred  _ = F

    fun	lambdaPred (vLambda _) = T
      | lambdaPred  _ = F

    fun	mkPred p se = let
	fun f (x as (h,(ref v))) k = ((p v) k x)
        in mBind f se 
        end

    val NilP = mkPred nilPred
    val BoolP = mkPred boolPred
    val IntP = mkPred intPred
    val PairP = mkPred pairPred
    val LambdaP = mkPred lambdaPred

    fun eval i (se:sexp) =
      (limit:= i; (se (fn x => x) ((0,[]),ref vNil)))


  end





