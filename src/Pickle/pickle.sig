(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07
 * GPL Licence
 *)

signature PICKLE =
  sig
    type 'a pu

    val pickle    : 'a pu -> 'a -> string
    val unpickle  : 'a pu -> string -> 'a

    type hce
    val empty_hce : unit -> hce
    val unpickle' : 'a pu -> hce -> string -> 'a * hce

    val word      : word pu
    val word32    : Word32.word pu
    val int       : int pu
    val int32     : Int32.int pu
    val bool      : bool pu
    val string    : string pu
    val char      : char pu
    val time      : Time.time pu
    val real      : real pu
    val unit      : unit pu
    val pairGen   : 'a pu * 'b pu -> ('a * 'b) pu
    val tup3Gen   : 'a pu * 'b pu * 'c pu -> ('a * 'b * 'c) pu
    val tup4Gen   : 'a pu * 'b pu * 'c pu * 'd pu -> ('a * 'b * 'c * 'd) pu
    val pairGen0  : 'a pu * 'b pu -> ('a * 'b) pu
    val tup3Gen0  : 'a pu * 'b pu * 'c pu -> ('a * 'b * 'c) pu
    val tup4Gen0  : 'a pu * 'b pu * 'c pu * 'd pu -> ('a * 'b * 'c * 'd) pu
    val refEqGen  : ('a ref * 'a ref -> bool) -> 'a -> 'a pu -> 'a ref pu
    val refGen    : 'a -> 'a pu -> 'a ref pu
    val ref0Gen   : 'a pu -> 'a ref pu
    val ref0GenPP : ('a ref -> string) -> 'a pu -> 'a ref pu
    val refOneGen : 'a pu -> 'a ref pu
    val ref0EqGen : ('a ref * 'a ref -> bool) -> 'a pu -> 'a ref pu
    val ref0EqGenPP : ('a ref -> string) -> ('a ref * 'a ref -> bool) -> 'a pu -> 'a ref pu
    val ref0ShGen : 'a pu -> 'a ref pu
    val listGen   : 'a pu -> 'a list pu
    val optionGen : 'a pu -> 'a option pu
    val vectorGen : 'a pu -> 'a Vector.vector pu
    val shareGen  : 'a pu -> 'a pu
    val enumGen   : string * ''a list -> ''a pu

    val dataGen   : string * ('a->int) * ('a pu -> 'a pu) list -> 'a pu
    val data2Gen  : string * ('a->int) * ('a pu * 'b pu -> 'a pu) list
	            * string * ('b->int) * ('a pu * 'b pu -> 'b pu) list
                    -> 'a pu * 'b pu

    val data3Gen  : string * ('a->int) * ('a pu * 'b pu * 'c pu -> 'a pu) list
	            * string * ('b->int) * ('a pu * 'b pu * 'c pu -> 'b pu) list
	            * string * ('c->int) * ('a pu * 'b pu * 'c pu -> 'c pu) list
                    -> 'a pu * 'b pu * 'c pu

    val con0      : 'a -> 'b -> 'a pu
    val con1      : ('a->'b) -> ('b->'a) -> 'a pu -> 'b pu

    val convert   : ('a->'b) * ('b->'a) -> 'a pu -> 'b pu
    val convert0  : ('a->'b) * ('b->'a) -> 'a pu -> 'b pu

    val cache     : string -> ('a -> 'b pu) -> 'a -> 'b pu
    val cache2    : string -> ('a -> 'b pu * 'c pu) -> 'a -> 'b pu * 'c pu

    val register  : string -> 'a list -> 'a pu -> 'a pu

    val registerEq: ('a*'a->bool) -> ('a->int)
	            -> string -> 'a list -> 'a pu -> 'a pu

    val hashCons  : 'a pu -> 'a pu
    val hashConsEq: ('a*'a->bool) -> 'a pu -> 'a pu

    val newHash      : ('a -> int) -> 'a pu -> 'a pu
    val combHash     : ('a -> int) -> 'a pu -> 'a pu
    val maybeNewHash : ('a -> int option) -> 'a pu -> 'a pu

    val debug     : string -> 'a pu -> 'a pu
    val nameGen   : string -> 'a pu -> 'a pu
    val comment   : string -> 'a pu -> 'a pu
    val checkUnpickle : ('a -> unit) -> 'a pu -> 'a pu
    val debugUnpickle : string -> 'a pu -> 'a pu
  end

(*
 ['a pu] parameterized type of a pair of a pickler and an unpickler.

 [word] pickler-unpickler pair for word values.

 [int] pickler-unpickler pair for int values.

 [bool] pickler-unpickler pair for bool values.

 [string] pickler-unpickler pair for string values.

 [char] pickler-unpickler pair for char values.

 [pairGen(pu1,pu2)] generates a pickler-unpickler for a pair given
 pickler-unpicklers (pu1 and pu2) for the two components of the pair.

 [refGen v pu] generates a ref-pickler given (1) a dummy value v of
 the argument type (to deal with cycles) and (2) a pickler-unpickler
 for the argument type.

 [ref0Gen pu] generates a ref-pickler given a pickler-unpickler
 for the argument type. The function assumes that the refs are
 not involved in cycles.

 [listGen pu] generates a pickler-unpickler for a list given a
 pickler-unpickler pu for the type of the elements.

 [optionGen pu] generates a pickler-unpickler for an option type given
 a pickler-unpickler pu for the argument type to the option type.

 [shareGen pu] given a pickler-unpickler for some type, the function
 generates a pickler-unpickler, which implements sharing of equivalent
 values (defined by structural equality).

 [enumGen cs] generates a pickler-unpickler for an enumeration
 datatype, given each value of the datatype.

 [dataGen (toInt, puCons)] generates a pickler-unpickler for a
 datatype given (1) a function toInt mapping a constructed value of
 the datatype into the position of the pickler-unpickler for the
 datatype in the puCons list and (2) a list of pickler-unpicklers for
 each of the constructors of the datatype. Notice that the
 pickler-unpickler for a contructor is parameterized over a
 pickler-unpickler for the entire datatype, so that a
 pickler-unpickler for a construtor may work for recursive
 datatypes. Also notice, that the dataGen function implements sharing
 (upto polymorphic equality) for the generated pickler-unpickler.

 [dataGen2 (aToInt, aPuCons, bToInt, bPuCons)] works as dataGen, but
 for two mutually recursive datatypes.

 [pickle pu v] returns the result of serializing (pickling) v.

 [unpickle pu s] returns the result of unpickling s.

 [unpickle' pu hce s] returns a pair of the result of unpickling s and
 an accumulated hashcons environment.

 [get is] reads a word from the instream is.

 [out (w,os)] writes a word to the outstream os.

 [register vs pu] returns a pickler-unpickler with the property
 that a pickled value equal to a value in vs is equal to the
 value in vs when unpickled.
*)
