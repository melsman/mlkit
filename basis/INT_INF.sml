signature INT_INF =
  sig
    include INTEGER
    
    val divmod  : (int * int) -> (int * int)
    val quotrem : (int * int) -> (int * int)
    val pow : (int * Int.int) -> int
    val log2 : int -> Int.int
    val hash : int -> Int.int
  end

(*
 The optional IntInf structure is one of the possible implementations
 of the INTEGER interface. In addition to the INTEGER operations, it
 provides some operations useful for programming with arbitrarily
 large integers. Note that operations in IntInf that return a value of
 type IntInf.int will never raise the Overflow exception.
*)