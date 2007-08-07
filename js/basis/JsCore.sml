signature JS_CORE =
  sig
    type 'a T
    val unit   : unit T
    val int    : int T
    val string : string T
    val fptr   : foreignptr T
    val bool   : bool T
    val option : 'a T -> 'a option T
    val ==>    : 'a T * 'b T -> ('a -> 'b) T

    val exec0 : {stmt: string,
                 res: 'b T} 
                -> unit -> 'b

    val exec1 : {stmt: string, 
                 arg1: string * 'a1 T, 
                 res: 'b T} 
                -> 'a1 -> 'b

    val exec2 : {stmt: string, 
                 arg1: string * 'a1 T, 
                 arg2: string * 'a2 T, 
                 res: 'b T} 
                -> 'a1 * 'a2 -> 'b

    val exec3 : {stmt: string, 
                 arg1: string * 'a1 T, 
                 arg2: string * 'a2 T, 
                 arg3: string * 'a3 T, 
                 res: 'b T} 
                -> 'a1 * 'a2 * 'a3 -> 'b

    val exec4 : {stmt: string, 
                 arg1: string * 'a1 T, 
                 arg2: string * 'a2 T, 
                 arg3: string * 'a3 T, 
                 arg4: string * 'a4 T, 
                 res: 'b T} 
                -> 'a1 * 'a2 * 'a3 * 'a4 -> 'b

    val call0 : string * 'b T -> unit -> 'b
    val call1 : string * 'a1 T * 'b T -> 'a1 -> 'b
    val call2 : string * 'a1 T * 'a2 T * 'b T -> 'a1*'a2 -> 'b
    val call3 : string * 'a1 T * 'a2 T * 'a3 T * 'b T -> 'a1*'a2*'a3 -> 'b
    val call4 : string * 'a1 T * 'a2 T * 'a3 T * 'a4 T * 'b T -> 'a1*'a2*'a3*'a4 -> 'b
  end

structure JsCore :> JS_CORE =
  struct
    type 'a T = unit
    val unit   : unit T = ()
    val int    : int T = ()
    val string : string T = ()
    val fptr   : foreignptr T = ()
    val bool   : bool T = ()
    val option : 'a T -> 'a option T = fn _ => ()
    val ==>    : 'a T * 'b T -> ('a -> 'b) T = fn _ => ()

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
        prim("execStmtJS", (stmt, String.concatWith "," [n1,n2], v1,v2))

    fun exec3 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               arg3=(n3:string,t3: 'a3 T), 
               res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3) : 'b =
        prim("execStmtJS", (stmt, String.concatWith "," [n1,n2,n3], v1,v2,v3))

    fun exec4 {stmt:string, 
               arg1=(n1:string,t1: 'a1 T), 
               arg2=(n2:string,t2: 'a2 T), 
               arg3=(n3:string,t3: 'a3 T), 
               arg4=(n4:string,t4: 'a4 T), 
               res: 'b T} (v1: 'a1, v2: 'a2, v3: 'a3, v4: 'a4) : 'b =
        prim("execStmtJS", (stmt, String.concatWith "," [n1,n2,n3,n4], v1,v2,v3,v4))

    fun call0 (f: string, tb: 'b T) () : 'b =
        prim("callJS", f)
    fun call1 (f: string, t1: 'a1 T, tb: 'b T) (v1:'a1) : 'b =
        prim("callJS", (f,v1))
    fun call2 (f: string, t1: 'a1 T, t2: 'a2 T, tb: 'b T) (v1:'a1,v2:'a2) : 'b =
        prim("callJS", (f,v1,v2))
    fun call3 (f: string, t1: 'a1 T, t2: 'a2 T, t3: 'a3 T, tb: 'b T) (v1:'a1,v2:'a2,v3:'a3) : 'b =
        prim("callJS", (f,v1,v2,v3))
    fun call4 (f: string, t1: 'a1 T, t2: 'a2 T, t3: 'a3 T, t4: 'a4 T, tb: 'b T) (v1:'a1,v2:'a2,v3:'a3,v4:'a4) : 'b =
        prim("callJS", (f,v1,v2,v3,v4))
  end
