signature NS_DB_BASIC =
  sig
    val seqNextvalExp : string -> string  (*construct new-sequence expression*)
    val seqCurrvalExp : string -> string  (*construct last-used sequence expression*)
    val fromDual      : string
    val sysdateExp    : string
    val beginTrans    : quot
    val endTrans      : quot
    val rollback      : quot
    val fromDate      : Date.date -> string
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

    (* [dmlDb (db,dml)] execute dml using database handle db. Raises
    Fail msg if dml is unsuccessful (msg contains the error message
    returned from the database). *)
    val dmlDb           : db * quot -> unit

    (* [dmlTransDb (db,f)] executes function f using handle db, which
    probably sends a series of SQL statements to the database. All SQL
    statements are one atomic transaction. If any statement fails or
    any exception is raised inside f, then the transaction is rolled
    back and the exception is raised. *)
    val dmlTransDb      : db * (db -> 'a) -> 'a

    (* [panicDmlDb db f_panic sql] same as dmlDb, except that on error
    function f_panic is executed. panicDmlDb returns unit and it only
    raises an exception if f_panic does. *)
    val panicDmlDb      : db -> (quot -> 'a) -> quot -> unit

    (* [panicDmlTransDb db f_panic f] same as dmlTransDb except that
    on error function f_panic is executed. panicDmlTransDb returns the
    value returned by f_panic unless f_panic raises an exception. *)
    val panicDmlTransDb : db -> (quot -> 'a) -> (db -> 'a) -> 'a

    (* [getCol (s,key)] returns the value affiliated with key in set
     s. Returns "##" if key is not in the set s. *)
    val getCol          : set * string -> string

    (* [getColOpt (s,key)] returns the value SOME v where v is
    affiliated with key in set s. NONE is returned if key is not in
    the set s, *)
    val getColOpt       : set * string -> string option

    (* [foldDb (db,f,b,sql)] executes SQL statements sql and folds over
    the result set. Raises Fail msg on fail *)
    val foldDb          : db * ((string->string)*'a->'a) * 'a * quot -> 'a

    val foldSetDb       : db * (set*'a->'a) * 'a * quot -> 'a
    val appDb           : db * ((string->string)->'a) * quot -> unit
    val listDb          : db * ((string->string)->'a) * quot -> 'a list
    val oneFieldDb      : db * quot -> string
    val zeroOrOneFieldDb: db * quot -> string option
    val oneRowDb        : db * quot -> string list
    val zeroOrOneRowDb  : db * quot -> string list option

    (* [dml sql] similar to dmlDb *)
    val dml           : quot -> unit

    (* [dmlTrans f] similar to dmlTransDb *)
    val dmlTrans      : (db -> 'a) -> 'a
    val maybeDml      : quot -> unit
    val panicDml      : (quot -> 'a) -> quot -> unit
    val errorDml      : (unit -> 'a) -> quot -> unit
    val panicDmlTrans : (quot -> 'a) -> (db -> 'a) -> 'a

    val fold          : ((string->string)*'a->'a) * 'a * quot -> 'a
    val foldSet       : (set*'a->'a) * 'a * quot -> 'a
    val app           : ((string->string)->'a) * quot -> unit
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
    val seqCurrvalExp : string -> string  (*construct last-used sequence expression*)
    val seqCurrval    : string -> int     (*obtain last-used sequence number from database*)
    val sysdateExp    : string            (*construct sysdate expression*)

    val qq  : string -> string  (* replace each quote (') with quote-quote ('') *)
    val qq' : string -> string  (* as qq, but encapsulated in quotes ('...') *)

    val toDate : string -> Date.date option
    val fromDate : Date.date -> string

    val valueList     : string list -> string
    val setList       : (string*string) list -> string

    val wrapDb : (db -> 'a) -> 'a
  end
