signature NS_DB_BASIC =
  sig
    val seqNextvalExp : string -> string
    val fromDual      : string
    val sysdateExp    : string
    val beginTrans    : quot
    val endTrans      : quot
    val roolback      : quot
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
    val dmlTransDb      : db * (db -> 'a) -> 'a
    val panicDmlDb      : db -> (quot -> 'a) -> quot -> unit
    val panicDmlTransDb : db -> (quot -> 'a) -> (db -> 'a) -> 'a
    val select          : db * quot -> set
    val getRow          : db * set -> status
    val foldDb          : db * ((string->string)*'a->'a) * 'a * quot -> 'a
    val listDb          : db * ((string->string)->'a) * quot -> 'a list
    val oneFieldDb      : db * quot -> string
    val zeroOrOneFieldDb: db * quot -> string option
    val oneRowDb        : db * quot -> string list
    val zeroOrOneRowDb  : db * quot -> string list option

    val dml           : quot -> status
    val dmlTrans      : (db -> 'a) -> 'a
    val maybeDml      : quot -> unit
    val panicDml      : (quot -> 'a) -> quot -> unit
    val errorDml      : (unit -> 'a) -> quot -> unit
    val panicDmlTrans : (quot -> 'a) -> (db -> 'a) -> 'a

    val fold          : ((string->string)*'a->'a) * 'a * quot -> 'a
    val list          : ((string->string)->'a) * quot -> 'a list
    val oneField      : quot -> string
    val zeroOrOneField: quot -> string option
    val oneRow        : quot -> string list
    val oneRow'       : ((string->string)->'a) * quot -> 'a
    val zeroOrOneRow  : quot -> string list option
    val zeroOrOneRow' : ((string->string)->'a) * quot -> 'a option
    val existsOneRow  : quot -> bool

    val seqNextvalExp : string -> string  (*construct new-sequence expression*)
    val seqNextval    : string -> int     (*obtain new sequence number from database*)
    val sysdateExp    : string            (*construct sysdate expression*)

    val qq  : string -> string  (* replace each quote (') with quote-quote ('') *)
    val qq' : string -> string  (* as qq, but encapsulated in quotes ('...') *)

    val toDate : string -> Date.date option

    val valueList     : string list -> string
    val setList       : (string*string) list -> string

    val wrapDb : (db -> 'a) -> 'a
  end
