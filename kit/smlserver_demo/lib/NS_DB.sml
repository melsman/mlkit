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

    (* Quotation support *)
    type quot = string frag list 

    val dmlDb           : db * quot -> status
    val select          : db * quot -> set
    val getRow          : db * set -> status
    val foldDb          : db * ((string->string)*'a->'a) * 'a * quot -> 'a
    val oneFieldDb      : db * quot -> string
    val zeroOrOneFieldDb: db * quot -> string option
    val oneRowDb        : db * quot -> string list
    val zeroOrOneRowDb  : db * quot -> string list option

    val dml           : quot -> status
    val maybeDml      : quot -> unit
    val panicDml      : (string * string -> 'a) -> quot -> unit

    val fold          : ((string->string)*'a->'a) * 'a * quot -> 'a
    val oneField      : quot -> string
    val zeroOrOneField: quot -> string option
    val oneRow        : quot -> string list
    val zeroOrOneRow  : quot -> string list option

    val seqNextvalExp : string -> string  (*construct new-sequence expression*)
    val seqNextval    : string -> int     (*obtain new sequence number from database*)

    val qq  : string -> string  (* replace each quote (') with quote-quote ('') *)
    val qq' : string -> string  (* as qq, but encapsulated in quotes ('...') *)
  end
