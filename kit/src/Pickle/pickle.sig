(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07 
 *)

signature PICKLE =
  sig
    type instream and outstream
	
    type 'a pickler   = 'a -> outstream -> outstream
    type 'a unpickler = instream -> 'a * instream

    type 'a pu

    val pickler   : 'a pu -> 'a pickler
    val unpickler : 'a pu -> 'a unpickler

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
    val refGen    : 'a pu -> 'a -> 'a ref pu
    val ref0Gen   : 'a pu -> 'a ref pu
    val listGen   : 'a pu -> 'a list pu
    val optionGen : 'a pu -> 'a option pu
    val vectorGen : 'a pu -> 'a Vector.vector pu
    val shareGen  : 'a pu -> 'a pu
    val enumGen   : ''a list -> ''a pu     

    val dataGen   : ('a->int) * ('a pu -> 'a pu) list -> 'a pu
    val data2Gen  : ('a->int) * ('a pu * 'b pu -> 'a pu) list 
	            * ('b->int) * ('a pu * 'b pu -> 'b pu) list 
                    -> 'a pu * 'b pu

    val data3Gen  : ('a->int) * ('a pu * 'b pu * 'c pu -> 'a pu) list 
	            * ('b->int) * ('a pu * 'b pu * 'c pu -> 'b pu) list 
	            * ('c->int) * ('a pu * 'b pu * 'c pu -> 'c pu) list 
                    -> 'a pu * 'b pu * 'c pu

    val con0       : 'b -> 'b pu -> 'b pu
    val con1       : ('a->'b) -> ('b->'a) -> 'a pu -> 'b pu

    val empty      : unit -> outstream
    val fromString : string -> instream
    val toString   : outstream -> string

    val convert    : ('a->'b) * ('b->'a) -> 'a pu -> 'b pu

    val cache      : ('a -> 'b pu) -> 'a -> 'b pu

    val register   : 'a list -> 'a pu -> 'a pu

    val registerEq : ('a*'a->bool) -> ('a->int) 
                     -> 'a list -> 'a pu -> 'a pu

    val debug      : string -> 'a pu -> 'a pu
  end

(*
 [instream] type of an instream.

 [outstream] type of an outstream.

 ['a pickler] parameterized pickler type.
 
 ['a unpickler] parameterized unpickler type.
 
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

 [empty()] returns an empty outstream.

 [fromString s] returns an instream corresponding to the string s.

 [toString os] returns a string corresponding to output sent to the
 outstream os.

 [get is] reads a word from the instream is.

 [out (w,os)] writes a word to the outstream os.

 [register vs pu] returns a pickler-unpickler with the property
 that a pickled value equal to a value in vs is equal to the
 value in vs when unpickled.
*)

