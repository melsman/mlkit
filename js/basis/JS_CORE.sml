(** Core Javascript functionality 

Basic functionality for accessing native Javascript code.
*)

signature JS_CORE =
  sig
    type 'a T
    val unit   : unit T
    val int    : int T
    val string : string T
    val real   : real T
    val fptr   : foreignptr T
    val bool   : bool T
    val option : 'a T -> 'a option T
    val ==>    : 'a T * 'b T -> ('a -> 'b) T
    val ===>   : 'a T * 'b T * 'c T -> ('a*'b -> 'c) T
    val ====>  : 'a T * 'b T * 'c T * 'd T -> ('a*'b*'c -> 'd) T

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

    val exec5 : {stmt: string, 
                 arg1: string * 'a1 T, 
                 arg2: string * 'a2 T, 
                 arg3: string * 'a3 T, 
                 arg4: string * 'a4 T, 
                 arg5: string * 'a5 T, 
                 res: 'b T} 
                -> 'a1 * 'a2 * 'a3 * 'a4 * 'a5 -> 'b

    val exec10 : {stmt: string, 
                  arg1: string * 'a1 T, 
                  arg2: string * 'a2 T, 
                  arg3: string * 'a3 T, 
                  arg4: string * 'a4 T, 
                  arg5: string * 'a5 T, 
                  arg6: string * 'a6 T, 
                  arg7: string * 'a7 T, 
                  arg8: string * 'a8 T, 
                  arg9: string * 'a9 T, 
                  arg10: string * 'a10 T, 
                  res: 'b T} 
                 -> 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 -> 'b

    val call0 : string * 'b T -> 'b
    val call1 : string * 'a1 T * 'b T -> 'a1 -> 'b
    val call2 : string * 'a1 T * 'a2 T * 'b T -> 'a1*'a2 -> 'b
    val call3 : string * 'a1 T * 'a2 T * 'a3 T * 'b T -> 'a1*'a2*'a3 -> 'b
    val call4 : string * 'a1 T * 'a2 T * 'a3 T * 'a4 T * 'b T -> 'a1*'a2*'a3*'a4 -> 'b

    val method0 : 'b T -> foreignptr -> string -> 'b
    val method1 : 'a1 T -> 'b T -> foreignptr -> string -> 'a1 -> 'b
    val method2 : 'a1 T -> 'a2 T -> 'b T -> foreignptr -> string -> 'a1 -> 'a2 -> 'b
    val method3 : 'a1 T -> 'a2 T -> 'a3 T -> 'b T -> foreignptr -> string -> 'a1 -> 'a2 -> 'a3 -> 'b
    val method4 : 'a1 T -> 'a2 T -> 'a3 T -> 'a4 T -> 'b T -> foreignptr -> string -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b

    val getProperty : foreignptr -> 'a T -> string -> 'a
    val setProperty : foreignptr -> 'a T -> string -> 'a -> unit

    structure Array : sig
      type t = foreignptr
      val empty    : unit -> t
      val push     : 'a T -> t -> 'a -> t
      val fromList : 'a T -> 'a list -> t
    end

    structure Object : sig
      type t = foreignptr
      val empty    : unit -> t
      val get      : 'a T -> t -> string -> 'a
      val set      : 'a T -> t -> string -> 'a -> unit
      val fromList : 'a T -> (string * 'a) list -> t
    end

    structure TypedObjects : sig
      type 'a j   (* any *)
      type 'a o   (* objects *)
      type 'a a   (* arrays *)
      val /> : 'a o j * 'b o j -> ('a->'b) o j
      val P : string -> 'a j -> 'a o j
      val S : string -> string j
      val I : int -> int j
      val R : real -> real j
      val B : bool -> bool j
      val F : foreignptr -> foreignptr j
      val A : 'a j list -> 'a a j
      val objToFptr : 'a o j -> foreignptr
      val arrToFptr : 'a a j -> foreignptr
    end
                           
  end

(**

[type 'a T]

[execN a v] returns the result of executing the javascript statement
in a with actual arguments (v) provided for the formal parameters,
specified in a.

[callN (t1,...,tn,tr)] (v1,...,vn)] returns the result of calling the
javascript function n with proper arguments (vi) given for each of the
arguments specified by tn.

[getProperty fp t s] extracts the property s (of type t) from the
object specified by fp.

[setProperty fp t s v] sets the property s to the value v (of type t)
on the object specified by fp.
*)
