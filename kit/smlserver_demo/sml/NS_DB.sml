signature NS_DB_BASIC =
  sig
    val seqNextvalExp : string -> string
    val fromDual      : string
  end

signature NS_POOL =
  sig
    type pool = string
    exception DbPoolError of string
      
    (* Pools are initially read from the nsd.tcl configuration file with          *)
    (*   initPools(sectionName, key), e.g., initPools("ns/server/nh/db", "Pools") *)
    (* where the initfile contains: ns_section "ns/server/nh/db"                  *)
    (*                              ns_param Pools main,sub                       *)
    (* Raises exception DbPoolError, if there are no pools specified              *)
    val initPools  : string * string -> unit
    val initPoolsL : string list -> unit
    val putPool    : pool -> unit
    val getPool    : unit -> pool
    val toList     : unit -> pool list
    val pp         : unit -> string
  end 

signature NS_DB =
  sig
    structure Pool : NS_POOL

    eqtype status
    type set
    type pool = Pool.pool
    type db

    val init           : string * string -> unit
    val poolGetHandle  : pool -> db
    val poolPutHandle  : db -> unit
    val getHandle      : unit -> db
    val putHandle      : db -> unit

    val dmlDb           : db * string -> status
    val select          : db * string -> set
    val getRow          : db * set -> status
    val foldDb          : db * ((string->string)*'a->'a) * 'a * string -> 'a
    val oneFieldDb      : db * string -> string
    val zeroOrOneFieldDb: db * string -> string option
    val oneRowDb        : db * string -> string list
    val zeroOrOneRowDb  : db * string -> string list option

    val dml           : string -> status
    val fold          : ((string->string)*'a->'a) * 'a * string -> 'a
    val oneField      : string -> string
    val zeroOrOneField: string -> string option
    val oneRow        : string -> string list
    val zeroOrOneRow  : string -> string list option

    val seqNextvalExp : string -> string  (*construct new-sequence expression*)
    val seqNextval    : string -> int     (*obtain new sequence number from database*)

    val qq  : string -> string  (* replace each quote (') with quote-quote ('') *)
    val qq' : string -> string  (* as qq, but encapsulated in quotes ('...') *)
  end
