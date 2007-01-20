signature WEB_DB_HANDLE = sig
  (* Database handles *)
  type db

  val getHandle       : unit -> db
  val putHandle       : db -> unit
  val wrapDb          : (db -> 'a) -> 'a

  (* Data manipulation language *)
  val dmlDb           : db -> quot -> unit
  val execDb          : db -> quot -> unit
  val panicDmlDb      : db -> (quot->'a) -> quot -> unit

  (* Transactions *)
  val dmlTransDb      : db -> (db -> 'a) -> 'a
  val dmlTrans        : (db -> 'a) -> 'a
  val panicDmlTransDb : db -> (quot->'a) -> (db->'a) -> 'a
  val panicDmlTrans   : (quot->'a) -> (db->'a) -> 'a

  (* Stored Procedure *)
  val execSpDb        : db -> quot list -> unit

  (* Queries *)
  val foldDb    : db -> ((string->string)*'a->'a) -> 'a -> quot -> 'a
  val foldDbCol : db -> (string list -> (string -> string option) * 'a 
                         -> 'a)
                  -> 'a -> quot -> 'a
  val appDb     : db -> ((string->string)->'a) -> quot -> unit
  val appDbCol  : db -> (string list -> (string->string option)->'a) 
                  -> quot -> unit
  val listDb    : db -> ((string->string)->'a) -> quot -> 'a list
  val listDbCol : db -> (string list -> (string->string option)->'a) 
                        -> quot -> 'a list
  val zeroOrOneRowDb  : db -> quot -> string list option
  val oneFieldDb      : db -> quot -> string 
  val zeroOrOneFieldDb: db -> quot -> string option
  val oneRowDb        : db -> quot -> string list
  val oneRowDb'       : db -> ((string->string)->'a) -> quot -> 'a
  val zeroOrOneRowDb' : db -> ((string->string)->'a) -> quot 
                        -> 'a option
  val existsOneRowDb  : db -> quot -> bool

  (* Sequences *)
  val seqNextvalDb    : db -> string -> int
  val seqCurrvalDb    : db -> string -> int
end

(*
 [db] type of database handle. Whenever the Web server talks to the
 database, it is by means of a database handle. Database handles are
 kept in the Web server using a prioritized set of pools. Each Web
 script obtains and releases database handles from the set of pools in
 a stack-like manner (each script may own at most one database handle
 from each pool). This arrangement is to avoid the possibility of
 deadlocks in case multiple Web scripts run simultaneously.

 [getHandle] returns a database handle from the next available
 pool. Raises Fail if no more pools are available.

 [putHandle db] returns the database handle db to its pool and makes
 the pool available to a subsequent call to getHandle.

 [initPools pools] initializes the set of pools. The pools must be
 defined in the nsd.tcl configuration file. See the file lib/Db.sml
 for a use of this function.

 [dmlDb db dml] executes the data manipulation language command dml
 using database handle db. Raises Fail msg if dml is unsuccessful; msg
 is the error message returned from the database. 

 [panicDmlDb db f sql] executes the data manipulation language command
 dml using database handle db. Calls the function f with with an error
 message as argument if the dml command is unsuccessful. panicDmlDb
 returns unit and raises an exception only if f does. 

 [dmlTransDb db f] executes function f using handle db, which may send
 a series of SQL statements to the database. All SQL statements are
 executed as one atomic transaction. If any statement fails or any
 exception is raised inside f, then the transaction is rolled back and
 the exception is raised.

 [dmlTrans f] similar to dmlTransDb, but with a database handle
 obtained from the next available pool.

 [panicDmlTransDb db f_panic f_db] same as dmlTransDb except that on
 error function f_panic is executed. panicDmlTransDb returns the value
 returned by f_panic unless f_panic raises an exception, in which case
 panicDmlTransDb raises this exception.

 [panicDmlTrans f_panic f_db] similar to panicDmlTransDb, but a
 database handle is obtained from the next available pool.

 [foldDb db f b sql] executes SQL statement sql and folds over the
 result set. b is the base and f is the fold function; the first
 argument to f is a function that maps column names to values. Raises
 Fail msg on error.

 [foldSetdb db f b sql] similar to foldDb except that f takes the
 result set as argument. Raises Fail msg on fail.

 [appDb db f sql] executes SQL statement sql and applies f on each row
 in the result set. Raises Fail on error.

 [listDb db f sql] executes SQL statement sql and applies f on each
 row in the result set. The result elements are returned as a
 list. Raises Fail on error.

 [zeroOrOneRowDb db sql] executes SQL statement that must return
 either zero or one row. Returns all columns as a list of
 strings. Raises Fail on error.

 [oneFieldDb db sql] executes SQL statement sql, which must return
 exactly one row with one column, which the function returns as a
 string. Raises Fail on error.

 [zeroOrOneFieldDb db sql] executes SQL statement sql, which must
 return either zero or one row. If one row is returned then there must
 be exactly one column in the row. Raises Fail on error.

 [oneRowDb db sql] executes SQL statement sql, which must return
 exactly one row. Returns all columns as a list of strings. Raises
 Fail on error.

 [oneRowDb' db f sql] executes SQL statement sql, which must return
 exactly one row. Returns f applied on the row. Raises Fail on error.
    
 [zeroOrOneRowDb' db f sql] executes SQL statement sql, which must
 return either zero or one row. Returns f applied on the row if it
 exists. Raises Fail on error.

 [existsOneRowDb db sql] executes SQL statement sql and returns true
 if one or more rows are returned; otherwise returns false. Raises
 Fail on error.

 [seqNextvalDb db seq_name] executes SQL statement using database
 handle db to generate a new number from sequence seq_name. Raise Fail
 on error.

 [seqCurrvalDb db seqName] executes SQL statement using database
 handle db to get the current number from sequence seq_name. Raises
 Fail on error.

 [wrapDb f] obtains a handle db with getHandle. applies f to db and
 before returning the result, the handle db is returned with
 putHandle.  
*)
