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
    val ===>    : 'a T * 'b T * 'c T-> ('a*'b -> 'c) T = fn _ => ()

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

    fun getProperty (fp: foreignptr) (t:'a T) (s:string) : 'a =
        prim("execStmtJS", ("return fp[s];", "fp,s", fp,s))

    fun setProperty (fp: foreignptr) (t:'a T) (s:string) (v:'a) : unit =
        prim("execStmtJS", ("fp[s] = v;", "fp,s,v", fp, s, v))

  end
