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
    val toTimestampExp: string -> string
    val timestampType : string
  end

signature NS_POOL =
  sig
    type pool = string

    (* [initPools (sectionName, key)] initialises the set of pools
    by looking in the nsd.tcl configuration file (e.g.,
    initPools("ns/server/nh/db", "Pools")), where the initfile
    contains: 
       ns_section "ns/server/nh/db" 
       ns_param Pools main,sub
    Raises exception Fail, if there are no pools specified *)
    val initPools  : string * string -> unit
 
    (* [initPoolsL ps] initializes the set of pools to ps. The pools
    must be defined in the nsd.tcl initialization file. See lib/Db.sml
    for an use of this function. *)
    val initPoolsL : string list -> unit

    (* [putPool p] put pool p back into the set of pools. *)
    val putPool    : pool -> unit

    (* [getPool ()] fetches a free pool from the set of pools. Raises
    Fail if no pools are available. *)
    val getPool    : unit -> pool

    (* [toList ()] returns the set of pools as a list. *)
    val toList     : unit -> pool list

    (* [pp ()] returns a pretty print of the set of pools. *)
    val pp         : unit -> string
  end 

signature NS_DB =
  sig
    structure Pool : NS_POOL

    eqtype status

    type set
    type pool = Pool.pool

    (* Database handle, abbreviated db *)
    type db

    (* Quotation support *)
    type quot = string frag list 

    (* [init (sectionName,key)] similar to Pool.initPools *)
    val init           : string * string -> unit

    (* [poolGetHandle p] returns a database handle from pool p. Raises
    Fail on error, (e.g., no handles available) *)
    val poolGetHandle  : pool -> db

    (* [poolPutHandle db] returns the database handle db to its pool
    for reuse (by a subsequent call to poolGetHandle). A database
    handle is member of one and only one pool. The pool is encoded in
    the database handle type db. *)
    val poolPutHandle  : db -> unit

    (* [getHandle] returns a database handle from the next available
    pool. Raises Fail on error (e.g., no pools available). *)
    val getHandle      : unit -> db

    (* [putHanlde db] returns db to its pool (see poolPutHandle) and
    makes the pool available to a subsequent call to getHandle. *)
    val putHandle      : db -> unit

    (***** SQL functions - Data Modification Language *****)

    (* [dmlDb (db,dml)] execute dml using database handle db. Raises
    Fail msg if dml is unsuccessful (msg contains the error message
    returned from the database). *)
    val dmlDb           : db * quot -> unit

    (* [dml sql] similar to dmlDb *)
    val dml             : quot -> unit

    (* [maybeDml sql] executes sql and returns the value unit. Does
    not raise Fail - errors are suppressed. This function is handy, if
    you insert a row, say r1, into a table where another row r2 exists
    with the same unique keys, and that is OK (i.e., you don't want to
    insert r1). *)
    val maybeDml      : quot -> unit

    (* [panicDmlDb db f_panic sql] same as dmlDb, except that on error
    function f_panic is executed. panicDmlDb returns unit and it only
    raises an exception if f_panic does. *)
    val panicDmlDb      : db -> (quot -> 'a) -> quot -> unit

    (* [panicDml f_panic sql] executes sql and returns the value
    unit. On error the function f_panic is applied to an error
    string. The function always returns unit. This function is handy
    if you want some panic action to be carried out on error. If you
    want the script to end on error, then f_panic must call Ns.exit()
    as it's last expression.*)
    val panicDml      : (quot -> 'a) -> quot -> unit

    (* [dmlTransDb (db,f_db)] executes function f_db using handle db,
    which probably sends a series of SQL statements to the
    database. All SQL statements are one atomic transaction. If any
    statement fails or any exception is raised inside f, then the
    transaction is rolled back and the exception is raised. *)
    val dmlTransDb      : db * (db -> 'a) -> 'a

    (* [dmlTrans f_db] similar to dmlTransDb *)
    val dmlTrans      : (db -> 'a) -> 'a

    (* [panicDmlTransDb db f_panic f_db] same as dmlTransDb except that
    on error function f_panic is executed. panicDmlTransDb returns the
    value returned by f_panic unless f_panic raises an exception. *)
    val panicDmlTransDb : db -> (quot -> 'a) -> (db -> 'a) -> 'a

    (* [panicDmlTrans f_panic f_db] similar to panicDmlTransDb *)
    val panicDmlTrans : (quot -> 'a) -> (db -> 'a) -> 'a

    (***** SQL functions - Select *****)

    (* [getCol (s,key)] returns the value affiliated with key in set
     s. Returns "##" if key is not in the set s. *)
    val getCol          : set * string -> string

    (* [getColOpt (s,key)] returns the value SOME v where v is
    affiliated with key in set s. NONE is returned if key is not in
    the set s, *)
    val getColOpt       : set * string -> string option

    (* [foldDb (db,f,b,sql)] executes SQL statement sql and folds over
    the result set. b is the base and f is the fold function; the
    first argument to f is a function that maps column names into
    values. Raises Fail msg on error. *) 
    val foldDb : db * ((string->string)*'a->'a) * 'a * quot -> 'a

    (* [fold (f,b,sql)] similar to foldDb *)
    val fold          : ((string->string)*'a->'a) * 'a * quot -> 'a

    (* [foldSetdb (db,f,b,sql)] similar to foldDb except that f
    takes the result set as argument. Raises Fail msg on fail *)
    val foldSetDb       : db * (set*'a->'a) * 'a * quot -> 'a

    (* [fold (f,b,sql)] similar to foldSetDb *)
    val foldSet       : (set*'a->'a) * 'a * quot -> 'a

    (* [appDb (db,f,sql)] executes SQL statement sql and applies f on
    each row in the result set. Raises Fail on error. *)
    val appDb           : db * ((string->string)->'a) * quot -> unit

    (* [app (f,sql)] similar to appDb *)
    val app           : ((string->string)->'a) * quot -> unit

    (* [listDb (db,f,sql)] executes SQL statement sql and applies f on
    each row in the result set. The result elements are returned as a
    list. Raises Fail on error. *)
    val listDb          : db * ((string->string)->'a) * quot -> 'a list

    (* [list (f,sql)] similar to listDb *)
    val list          : ((string->string)->'a) * quot -> 'a list

    (* [oneFieldDb (db,sql)] executes SQL statement sql which must
    return exactly one row with one column - that string is
    returned. Raises Fail on error *)
    val oneFieldDb      : db * quot -> string

    (* [oneField sql] similar to oneFieldDb *)
    val oneField      : quot -> string

    (* [zeroOrOneFieldDb (db,sql) executes SQL statement sql which
    must either return zero or one row. If one row is returned then
    there must be exactly one column in the row. Raises Fail on
    error. *)
    val zeroOrOneFieldDb: db * quot -> string option

    (* [zeroOrOneField sql] similar to zeroOrOneFieldDb *)
    val zeroOrOneField: quot -> string option

    (* [oneRowDb (db,sql) executes SQL statement which must return
    exactly one row. Returns all columns as a list of strings. Raises
    Fail on error. *)
    val oneRowDb        : db * quot -> string list

    (* [oneRow sql] similar to oneRowDb *)
    val oneRow        : quot -> string list

    (* [oneRowDb' (db,f,sql) executes SQL statement which must return
    exactly one row. Returns f applied on the row. Raises Fail on
    error. *)
    val oneRowDb'       : db * ((string->string)->'a) * quot -> 'a

    (* [oneRow' (f,sql)] similar to oneRowDb' *)
    val oneRow'       : ((string->string)->'a) * quot -> 'a

    (* [zeroOrOneRowDb (db,sql) executes SQL statement that must
    return either zero or one row. Returns all columns as a list of
    strings. Raises Fail on error. *)
    val zeroOrOneRowDb  : db * quot -> string list option

    (* [zeroOrOneRow sql] similar to zeroOrOneRowDb *)
    val zeroOrOneRow  : quot -> string list option

    (* [zeroOrOneRowDb' (db,f,sql) executes SQL statement that must
    return either zero or one row. Returns f applied on the row if
    exists Raises Fail on error. *)
    val zeroOrOneRowDb'  : db * ((string->string)->'a) * quot -> 'a option

    (* [zeroOrOneRow'] (f,sql) similar to zeroOrOneRowDb' *)
    val zeroOrOneRow' : ((string->string)->'a) * quot -> 'a option

    (* [existsOneRowDb (db,sql)] executes sql and returns true if one
    or more rows are returned; otherwise returns false. Raises Fail on
    error *)
    val existsOneRowDb : db * quot -> bool

    (* [existsOneRow sql] similar to existsOneRowDb *)
    val existsOneRow  : quot -> bool

    (***** SQL functions - Sequences *****)

    (* [seqNextvalExp seq_name] returns a string to fit in an SQL
    statement generating a new number from sequence seq_name.*)
    val seqNextvalExp : string -> string

    (* [seqNextvalDb (db,seq_name)] executes SQL statement using
    database handle db to generate a new number from sequence
    seq_name. Raise Fail on error *)
    val seqNextvalDb  : (db * string) -> int

    (* [seqNextval seq_name] similar to seqNextvalDb *)
    val seqNextval    : string -> int

    (* [seqCurrvalExp seq_name] returns a string to fit in an SQL
    statement returning the current number from the sequence
    seq_name. *)
    val seqCurrvalExp : string -> string

    (* [seqCurrvalDb (db,seqName)] executes SQL statement using
    database handle db to get the current number from sequence
    seq_name. Raises Fail on error *)
    val seqCurrvalDb: (db * string) -> int

    (* [seqCurrval seqName] similar to seqCurrvalDb *)
    val seqCurrval    : string -> int

    (***** SQL functions - Miscellaneous *****)

    (* [sysdateExp] returns a string to fit in an SQL statement
    returning the current date *)
    val sysdateExp    : string

    (* [qq v] returns a string with each quote (') replaced by
    double quotes ('') *)
    val qq  : string -> string

    (* [qq' v] similar to qq except that the result is encapsulated by
    quotes: say v1 = qq v then qq' v = 'v1' *)
    val qq' : string -> string

    (* [toDate d] returns the Date.date representation of d, where d
    is the date representation from the database. Returns NONE if d
    cannot be converted into a Date.date. Only year, month and day are
    considered. *)
    val toDate : string -> Date.date option

    (* [timestampType] the database type representing a timestamp. *)
    val timestampType : string

    (* [toTimestampExp d] returns a string that fit in a select
    statement which will return a timestamp representation of column d
    which can be used by toTimestamp:

       `select ^(Db.toTimestampExp "d") from t`

    where d is a column of type date (in oracle) or datatime (in
    PostgreSQL and MySQL) *)
    val toTimestampExp: string -> string

    (* [toTimestamp t] returns the Date.date representation of t,
    where d is the timestap representation from the database. Returns
    NONE if t cannot be converted into a Date.date. Year, month, day,
    hour, minutes and seconds are considered. *)
    val toTimestamp : string -> Date.date option

    (* [fromDate d] returns a string which can be used in an SQL
    statement to insert the date d in the database *)
    val fromDate : Date.date -> string

    (* [valueList vs] returns a string formatted to be part of an
    insert statement:

      `insert into t (f1,f2,f3) values (^(Db.valueList [f1,f2,f3]))`

    is turned into

      `insert into t (f1,f2,f3) values ('f1_','f2_','f3_')`

    where fi_ are the properly quoted values. *)
    val valueList     : string list -> string

    (* [setList nvs] returns a string formatted to be part of an
    update statement. Say nvs = [(n1,v1),(n2,v2)], then

       `update t set ^(Db.setList nvs)`
 
    is turned into
   
       `update t set n1='v1_',n2='v2_'`

    where vi_ are the properly quoted values *)
    val setList : (string*string) list -> string

    (* [wrapDb f] obtains a handle db with getHandle. applies f to db
    and before returning the result, the handle db is returned with
    putHandle *)
    val wrapDb : (db -> 'a) -> 'a
  end
