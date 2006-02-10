signature WEB_DB =
  sig
    structure Handle : WEB_DB_HANDLE

    (* init *)

    type 'a Type

    val config : 'a Type * string * 'a -> unit

    (* Data manipulation language *)
    val dml           : quot -> unit
    val exec          : quot -> unit
    val maybeDml      : quot -> unit
    val panicDml      : (quot -> 'a) -> quot -> unit

    (* Stored Procedure *)
    val execSp        : quot list -> unit

    (* Queries *)
    val fold          : ((string->string)*'a->'a) -> 'a 
                        -> quot -> 'a
    val foldCol       : (string list -> (string->string option)*'a->'a) -> 'a 
                        -> quot -> 'a
    val app           : ((string->string)->'a) -> quot 
                        -> unit
    val appCol        : (string list -> (string->string option)->'a) -> quot 
                        -> unit
    val list          : ((string->string)->'a) -> quot 
                        -> 'a list
    val listCol       : (string list -> (string->string option)->'a) -> quot 
                        -> 'a list
    val oneField      : quot -> string 
    val zeroOrOneField: quot -> string option
    val oneRow        : quot -> string list
    val oneRow'       : ((string->string)->'a) -> quot -> 'a
    val zeroOrOneRow  : quot -> string list option
    val zeroOrOneRow' : ((string->string)->'a) -> quot 
                        -> 'a option
    val existsOneRow  : quot -> bool

    (* Sequences *)
    val seqNextvalExp : string -> string
    val seqNextval    : string -> int
    val seqCurrvalExp : string -> string
    val seqCurrval    : string -> int

    (* Miscellaneous *)
    val sysdateExp    : string
    val qq            : string -> string
    val qqq           : string -> string
    val toDate        : string -> Date.date option
    val timestampType : string
    val toTimestampExp: string -> string
    val toTimestamp   : string -> Date.date option
    val fromDate      : Date.date -> string
    val toDateExp     : string -> string
    val valueList     : string list -> string
    val setList       : (string*string) list -> string
    val toBool        : string -> bool option
    val fromBool      : bool -> string
    val toReal        : string -> real option
    val fromReal      : real -> string

  end

(*
 [dml sql] executes the data manipulation language command 
 sql using a database handle obtained from the next pool. 
 Raises Fail msg if sql is unsuccessful; msg is the error 
 message returned from the database.

 [maybeDml sql] executes sql and returns the value unit. Does
 not raise Fail - errors are suppressed.

 [panicDml f sql] executes sql and returns the value unit. On 
 error the function f is applied to an error string. The 
 function always returns unit.

 [fold f b sql] executes SQL statement sql and folds over the 
 result set. b is the base and f is the fold function; the 
 first argument to f is a function that maps column names to 
 values. Raises Fail msg on error.

 [foldSet f b sql] similar to fold except that f takes the 
 result set as argument. Raises Fail msg on fail.

 [app f sql] executes SQL statement sql and applies f on each 
 row in the result set. Raises Fail on error.

 [list f sql] executes SQL statement sql and applies f on
 each row in the result set. The result elements are returned 
 as a list. Raises Fail on error.

 [oneField sql] executes SQL statement sql, which must return 
 exactly one row with one column, which the function returns 
 as a string. Raises Fail on error.

 [zeroOrOneField sql] executes SQL statement sql, which must 
 return either zero or one row. If one row is returned then
 there must be exactly one column in the row. Raises Fail on
 error.

 [oneRow sql] executes SQL statement sql, which must return
 exactly one row. Returns all columns as a list of strings. 
 Raises Fail on error.

 [oneRow' f sql] executes SQL statement sql, which must 
 return exactly one row. Returns f applied on the row. Raises 
 Fail on error.

 [zeroOrOneRow sql] executes SQL statement sql, which must
 return either zero or one row. Returns all columns as a list 
 of strings. Raises Fail on error.

 [zeroOrOneRow' f sql] executes SQL statement sql, which must
 return either zero or one row. Returns f applied on the row 
 if a row exists. Raises Fail on error.

 [existsOneRow sql] executes SQL statement sql and returns 
 true if the query results in one or more rows; otherwise 
 returns false. Raises Fail on error.

 [seqNextvalExp seq_name] returns a string to fit in an SQL
 statement generating a new number from sequence seq_name.

 [seqNextval seq_name] executes SQL statement to generate a 
 new number from sequence seq_name. Raise Fail on error.

 [seqCurrvalExp seq_name] returns a string to fit in an SQL
 statement returning the current number from the sequence
 seq_name.

 [seqCurrval seqName] executes SQL statement to get the 
 current number from sequence seq_name. Raises Fail on 
 error.

 [sysdateExp] returns a string representing the current date
 to be used in an SQL statement (to have your application 
 support different database vendors).

 [qq v] returns a string with each quote (') replaced by 
 double quotes ('') (e.g., qq("don't go") = "don''t go").

 [qqq v] similar to qq except that the result is encapsulated 
 by quotes (e.g., qqq("don't go") = "'don''t go'").

 [toDate d] returns the Date.date representation of d, where 
 d is the date representation used in the particular 
 database. Returns NONE if d cannot be converted into a 
 Date.date. Only year, month and day are considered.

 [toBool b] returns the Bool.bool representation of a boolean,
 where b is the bool representation used in the particular
 database. Returns NONE if b cannot be converted into a Bool.bool.

 [timestampType] returns the database type (as a string) 
 representing a timestamp (to have your application support 
 different database vendors).

 [toTimestampExp d] returns a string to put in a select
 statement, which will return a timestamp representation of 
 column d. Example: `select ^(Db.toTimestampExp "d") from t` 
 where d is a column of type date (in oracle) or datatime (in 
 PostgreSQL and MySQL).

 [toTimestamp t] returns the Date.date representation of t,
 where d is the timestap representation from the database. 
 Returns NONE if t cannot be converted into a Date.date. 
 Year, month, day, hour, minutes and seconds are considered.

 [fromDate d] returns a string to be used in an SQL statement 
 to insert the date d in the database.

 [fromBool b] returns a Bool.bool used in an SQL statement to 
 insert a bool b in the database

 [valueList vs] returns a string formatted to be part of an
 insert statement:

       `insert into t (f1,f2,f3) 
        values (^(Db.valueList [f1,f2,f3]))`

 is turned into

      `insert into t (f1,f2,f3) 
       values ('f1_','f2_','f3_')`

 where fi_ are the properly quoted values.

 [setList nvs] returns a string formatted to be part of an
 update statement. Say nvs = [(n1,v1),(n2,v2)], then

       `update t set ^(Db.setList nvs)`
 
 is turned into
   
       `update t set n1='v1_',n2='v2_'`

 where vi_ are the properly quoted values.
*)
