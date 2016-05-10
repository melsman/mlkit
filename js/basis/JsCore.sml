structure JsCore :> JS_CORE =
  struct
    type 'a T = unit
    val unit   : unit T = ()
    val int    : int T = ()
    val string : string T = ()
    val real   : real T = ()
    val fptr   : foreignptr T = ()
    val bool   : bool T = ()
    val option : 'a T -> 'a option T = fn _ => ()
    val ==>    : 'a T * 'b T -> ('a -> 'b) T = fn _ => ()
    val ===>   : 'a T * 'b T * 'c T-> ('a*'b -> 'c) T = fn _ => ()
    val ====>  : 'a T * 'b T * 'c T * 'd T -> ('a*'b*'c -> 'd) T = fn _ => ()

    fun exec0 {stmt:string, 
               res: 'b T} () : 'b =
        prim("execStmtJS", (stmt,""))

    fun exec1 {stmt:string, 
               arg1=(n1:string, t1: 'a1 T), 
               res: 'b T} (v1: 'a1) : 'b =
        prim("execStmtJS", (stmt,n1,v1))

    fun exec2 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               res: 'b T} (v1: 'a1, v2: 'a2) : 'b =
        prim("execStmtJS", (stmt, n1 ^ "," ^ n2, v1,v2))

    fun exec3 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               arg3=(n3:string,t3: 'a3 T), 
               res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3) : 'b =
        prim("execStmtJS", (stmt, n1 ^ "," ^ n2 ^ "," ^ n3, v1,v2,v3))

    fun exec4 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               arg3=(n3:string,t3: 'a3 T), 
               arg4=(n4:string,t4: 'a4 T), 
               res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3, v4: 'a4) : 'b =
        prim("execStmtJS", (stmt, n1 ^ "," ^ n2 ^ "," ^ n3 ^ "," ^ n4, v1,v2,v3,v4))

    fun exec5 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               arg3=(n3:string,t3: 'a3 T), 
               arg4=(n4:string,t4: 'a4 T), 
               arg5=(n5:string,t5: 'a5 T), 
               res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3, v4: 'a4, v5: 'a5) : 'b =
        prim("execStmtJS", (stmt, n1 ^ "," ^ n2 ^ "," ^ n3 ^ "," ^ n4 ^ "," ^ n5, v1,v2,v3,v4,v5))

    fun exec10 {stmt:string,
                arg1=(n1:string,t1: 'a1 T),
                arg2=(n2:string,t2: 'a2 T),
                arg3=(n3:string,t3: 'a3 T),
                arg4=(n4:string,t4: 'a4 T),
                arg5=(n5:string,t5: 'a5 T),
                arg6=(n6:string,t6: 'a6 T),
                arg7=(n7:string,t7: 'a7 T),
                arg8=(n8:string,t8: 'a8 T),
                arg9=(n9:string,t9: 'a9 T),
                arg10=(n10:string,t10: 'a10 T),
                res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3, v4: 'a4, v5: 'a5,
                            v6: 'a6, v7: 'a7, v8: 'a8, v9: 'a9, v10: 'a10) : 'b =
        prim("execStmtJS", (stmt, n1 ^ "," ^ n2 ^ "," ^ n3 ^ "," ^ n4 ^ "," ^ n5 ^ "," ^
                                  n6 ^ "," ^ n7 ^ "," ^ n8 ^ "," ^ n9 ^ "," ^ n10,
                            v1,v2,v3,v4,v5,v6,v7,v8,v9,v10))

    fun call0 (f: string, tb: 'b T) : 'b =
        prim("callJS", f)
    fun call1 (f: string, t1: 'a1 T, tb: 'b T) (v1:'a1) : 'b =
        prim("callJS", (f,v1))
    fun call2 (f: string, t1: 'a1 T, t2: 'a2 T, tb: 'b T) (v1:'a1,v2:'a2) : 'b =
        prim("callJS", (f,v1,v2))
    fun call3 (f: string, t1: 'a1 T, t2: 'a2 T, t3: 'a3 T, tb: 'b T) (v1:'a1,v2:'a2,v3:'a3) : 'b =
        prim("callJS", (f,v1,v2,v3))
    fun call4 (f: string, t1: 'a1 T, t2: 'a2 T, t3: 'a3 T, t4: 'a4 T, tb: 'b T) (v1:'a1,v2:'a2,v3:'a3,v4:'a4) : 'b =
        prim("callJS", (f,v1,v2,v3,v4))

    fun method0 tr obj m =
        exec1{arg1=("obj",fptr),res=tr,stmt="return obj." ^ m ^ "();"} obj

    fun method1 ta tr obj m a =
        exec2{arg1=("obj",fptr),arg2=("a",ta),res=tr,stmt="return obj." ^ m ^ "(a);"} (obj,a)

    fun method2 ta1 ta2 tr obj m a1 a2 =
        exec3{arg1=("obj",fptr),arg2=("a1",ta1),arg3=("a2",ta2),res=tr,
              stmt="return obj." ^ m ^ "(a1,a2);"} (obj,a1,a2)

    fun method3 ta1 ta2 ta3 tr obj m a1 a2 a3 =
        exec4{arg1=("obj",fptr),arg2=("a1",ta1),arg3=("a2",ta2),arg4=("a3",ta3),res=tr,
              stmt="return obj." ^ m ^ "(a1,a2,a3);"} (obj,a1,a2,a3)

    fun method4 ta1 ta2 ta3 ta4 tr obj m a1 a2 a3 a4 =
        exec5{arg1=("obj",fptr),arg2=("a1",ta1),arg3=("a2",ta2),arg4=("a3",ta3),arg5=("a4",ta4),res=tr,
              stmt="return obj." ^ m ^ "(a1,a2,a3,a4);"} (obj,a1,a2,a3,a4)

    fun getProperty (fp: foreignptr) (t:'a T) (s:string) : 'a =
        prim("execStmtJS", ("return fp[s];", "fp,s", fp,s))

    fun setProperty (fp: foreignptr) (t:'a T) (s:string) (v:'a) : unit =
        prim("execStmtJS", ("fp[s] = v;", "fp,s,v", fp, s, v))

    structure Array = struct
        type t = foreignptr
        val empty : unit -> t = exec0{stmt="return new Array();",res=fptr}
        fun push t a v =
            (exec2{stmt="a.push(v);",arg1=("a",fptr),arg2=("v",t),res=unit} (a,v); 
             a)
        fun fromList t = List.foldl (fn (v,a) => push t a v) (empty())
    end

    structure Object = struct
        type t = foreignptr
        val empty : unit -> t = exec0{stmt="return {};",res=fptr}
        fun get t obj p = getProperty obj t p
        fun set t obj p v = setProperty obj t p v
        fun fromList t = List.foldl (fn ((k,v),a) => (set t a k v; a)) (empty())
    end

    structure TypedObjects = struct
        datatype j0 =
                 S of string
                 | I of int
                 | R of real
                 | B of bool
                 | F of foreignptr
                 | A of j0 list
                 | P0 of string * j0
                 | /> of j0 * j0
        type 'a j = j0
        type obj = unit
        type 'a arr = unit
        fun P x y = P0 (x,y)
                       
        structure JArr = Array
        structure JObj = Object
                             
        fun objToFptr j =
            let val obj = JObj.empty()
            in setProps obj j
             ; obj
            end
        and setProps obj j =
            case j of
                P0(p,j) => setPropObj obj p j
              | />(j1,j2)  => (setProps obj j1; setProps obj j2)
              | _ => raise Fail "setProps expecting P or />"
        and setPropObj obj p j =
            case j of
                S s => JObj.set string obj p s
              | B b => JObj.set bool obj p b
              | I i => JObj.set int obj p i
              | R r => JObj.set real obj p r
              | F f => JObj.set fptr obj p f
              | P0 _ => setPropObj obj p (F(objToFptr j))
              | /> _ => setPropObj obj p (F(objToFptr j))
              | A _ => setPropObj obj p (F(arrToFptr j))
        and arrToFptr j =
            case j of
                A js =>
                let val arr = JArr.empty()
                in List.app (pushArr arr) js
                 ; arr
                end
              | _ => raise Fail "arrToFptr expects array"
        and pushArr arr j =
            case j of
                S s => (JArr.push string arr s; ())
              | B b => (JArr.push bool arr b; ())
              | I i => (JArr.push int arr i; ())
              | R r => (JArr.push real arr r; ())
              | F f => (JArr.push fptr arr f; ())
              | A _ => pushArr arr (F(arrToFptr j))
              | _ => pushArr arr (F(objToFptr j))
    
    end
                           
  end
