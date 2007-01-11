
signature WEB_DB_BASIC =
  sig
    val seqNextvalExp : string -> string  (*construct new-sequence expression*)
    val seqCurrvalExp : string -> string  (*construct last-used sequence expression*)
    val fromDual      : string
    val sysdateExp    : string
    val fromDate      : Date.date -> string
    val toDateExp     : string -> string
    val toTimestampExp: string -> string
    val timestampType : string
  end

structure NsDbBasicOra : WEB_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = seq_name ^ ".nextval"
    fun seqCurrvalExp seq_name = seq_name ^ ".currval"
    val fromDual = "from dual"
    val sysdateExp = "sysdate"
    fun fromDate d = "to_date('" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "','YYYY-MM-DD HH24:MI:SS')"
    fun toTimestampExp d = "to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"
    fun toDateExp n = "to_char(" ^ n ^ ",'YYYY-MM-DD')"
    val timestampType = "date"
  end

structure NsDbBasicPG : WEB_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = "nextval('" ^ seq_name ^ "')"
    fun seqCurrvalExp seq_name = "currval('" ^ seq_name ^ "')"
    val fromDual = ""
    val sysdateExp = "now()"
    fun fromDate d = "'" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "'"
    fun toDateExp n = "to_char(" ^ n ^ ",'YYYY-MM-DD')"  (* Needs testing *)
    fun toTimestampExp d = "to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"
    val timestampType = "timestamp"
  end

structure NsDbBasicMySQL : WEB_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = "null"
    fun seqCurrvalExp seq_name = raise Fail "seqCurrvalExp not supported on MySQL"
    val fromDual = ""
    val sysdateExp = "now()"
    fun fromDate d = "'" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "'"
    fun toDateExp n = n (* Needs testing *)
    fun toTimestampExp d = d (*"to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"*)
    val timestampType = "datetime"
  end


signature WEB_DB_BACKEND = 
  sig
    type DbHandle
    type DbResultSet
    type 'a Type
    structure DbBasic : WEB_DB_BASIC
    val config        : int -> ('a Type * string * 'a) -> unit
    val getHandle     : int -> DbHandle
    val putHandle     : DbHandle -> unit
    val dmlDb         : DbHandle -> quot -> unit
	  val execDb        : DbHandle -> quot -> unit
    val selectDb      : DbHandle -> quot -> (DbResultSet * string list)
	  val getRowDb      : DbResultSet -> (string -> (string option)) option
	  val getRowListDb  : DbResultSet -> (string option list) option
    val getRowListDb2 : DbResultSet -> (string list * (string option list) option)
    val dmlTransDb    : DbHandle -> (DbHandle -> 'a) -> 'a
  end

structure DbDummyBackend :> WEB_DB_BACKEND =
  struct 
    type DbHandle = int
    type DbResultSet = int
    type 'a Type = unit
    structure DbBasic = NsDbBasicOra
    fun config _ = raise Fail("This is a dummy database driver")
    val getHandle = config
    val putHandle = getHandle
    val dmlDb = getHandle
    val execDb = getHandle
    val selectDb = getHandle
    val getRowDb = getHandle
    val getRowListDb = getHandle
    val getRowListDb2 = getHandle
    val dmlTransDb = getHandle
  end

functor DbODBCBackend(type conn = int
                        val getReqRec : unit -> conn
                        val log : string -> string
                        val isNull : string -> bool
                        structure Info : WEB_INFO
                        structure Dynlib : WEB_DYNLIB
                        ) :> 
                               WEB_DB_BACKEND where type 'a Type = 'a Info.Type.Type =
  struct 
    type DbHandle = int option ref
    type DbResultSet = DbHandle * (string option list -> string -> string option) * string list
    type 'a Type = 'a Info.Type.Type
    structure DbBasic = NsDbBasicPG
    val first = ref true
    fun config (i:int) (t : 'a Info.Type.Type, d : string, v : 'a) : unit = 
                           let fun handRes (res : int) = case res 
                                                     of 0 => ()
                                                      | 1 => raise Fail "ODBC Driver: Unknown option"
                                                      | 2 => raise Fail "ODBC Driver: Out of memory"
                                                      | 3 => raise Fail "ODBC Driver: MaximumNumberOfConnections must be larger than SessionMaxDepth" 
                                                      | _ => raise Fail "ODBC Driver: Unknown problem in oracle driver"

                           
                               fun setInt (p : int) : unit = 
                                     if #name t = #name Info.Type.Int
                                     then 
                                     let 
                                       val res : int = 
                                        prim("@:", ("apsmlODBCSetVal",i,getReqRec(),p,
                                                (#from_string Info.Type.Int (#to_string t v))))
                                       in handRes res
                                       end
                                     else raise Domain
                               fun setString (p : int) : unit = 
                                     if #name t = #name Info.Type.String
                                     then 
                                       let val res : int = prim("@:", ("apsmlODBCSetVal",i,getReqRec(),p,
                                                    (#from_string Info.Type.String (#to_string t v))))
                                       in handRes res
                                       end
                                     else raise Domain
                               fun setBool (p : int) : unit = 
                                     if #name t = #name Info.Type.Bool
                                     then 
                                       let val res : int = prim("@:", ("apsmlODBCSetVal",i,getReqRec(),p,
                                                    if (#from_string Info.Type.Bool (#to_string t v))
                                                    then 1 
                                                    else 0))
                                       in handRes res
                                       end
                                     else raise Domain
                           in ((
                               if !first then 
                               (let 
                                   val b = Dynlib.dlopen (SOME "libsmlodbc.so", Dynlib.NOW, false)
                                   val _ = 
                                          (log "Opened libsmlodbc.so, performing resolvation" ;
                                           Dynlib.dlsym("apsmlODBCGetSession","apsmlODBCGetSession",b);
                                           Dynlib.dlsym("apsmlODBCDropSession", "apsmlODBCDropSession",b);
                                           Dynlib.dlsym("DBODBCExecuteSQL", "DBODBCExecuteSQL",b);
                                           Dynlib.dlsym("apsmlODBCGetCNames", "apsmlODBCGetCNames",b);
                                           Dynlib.dlsym("apsmlODBCGetRow","apsmlODBCGetRow",b);
                                           Dynlib.dlsym("DBODBCTransStart", "DBODBCTransStart", b);
                                           Dynlib.dlsym("DBODBCTransCommit", "DBODBCTransCommit",b);
                                           Dynlib.dlsym("DBODBCTransRollBack","DBODBCTransRollBack",b);
                                           Dynlib.dlsym("apsmlODBCSetVal","apsmlODBCSetVal",b);
                                           log "resolvation done")
                               in first:=false
                               end) handle Fail x => (log x; raise Fail x)
                               else ()
                              ) ;
                           case d 
                           of 
                              "UserName" => setString 2
                            | "PassWord" => setString 3
                            | "DSN"  => setString 4
                            | "SessionMaxDepth" => setInt 5
                            | _ => (log "Unknown setting :" ^ d; raise Domain))
                           end
    fun getHandle (i:int) : DbHandle = let val res : int = (log "apsmlODBCGetSession" ; prim(":", ("apsmlODBCGetSession",i,getReqRec()))) before (log("apsmlODBCGetSession DONE");())
                                 in if res = 0
                                    then raise Fail "ODBC Driver: Could not get database session"
                                    else (if res = 1 
                                    then raise Fail "ODBC Driver: Maximum nesting of odbc connections reached"
                                    else ref (SOME res))
                                 end
    fun putHandle (h:DbHandle) : unit = 
               case !h of NONE => ()
                        | SOME  r => (log "apsmlODBCDropSession" ; prim(":", ("apsmlODBCDropSession",r : int,getReqRec()));
                                      h:= NONE)
    fun dmlDb (h : DbHandle) q : unit = 
       case !h of NONE => raise Fail "ODBC Driver: Abuse, session closed"
                | SOME r => let
                              val sql = Quot.toString q
                              val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_DML")
                                      of SOME true => ignore (log sql)
                                       | _ => () 
                              val  res : int = prim("@:", ("DBODBCExecuteSQL",r : int, sql : string, getReqRec()))
                            in if res <> 2
                               then (* not DBDml *) 
                                  raise Fail ("ODBC Driver: dml: " ^ Quot.toString q ^ " failed")
                               else ()
                            end
    fun execDb (h : DbHandle) q : unit = 
       case !h of NONE => raise Fail "ODBC Driver: Abuse, session closed"
                | SOME r => let
                              val sql = Quot.toString q
                              val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_EXEC")
                                      of SOME true => ignore (log sql)
                                       | _ => () 
                              val res : int = prim("@:", ("DBODBCExecuteSQL",r,sql:string, getReqRec()))
                            in if res = 0
                               then (* DBError *) 
                                  raise Fail ("ODBC Driver: exec: " ^ sql ^ " failed")
                               else ()
                            end
    val toLower = String.map Char.toLower
    fun selector ([] : string list)  ([] : string option list)  (x : string) = NONE : string option
      | selector [] (r::rr) x = raise Fail "ODBC Driver: ODBC driver corruption"
      | selector (h::hr) [] x = raise Fail "ODBC Driver: ODBC driver corruption" 
      | selector (h::hr) (r::rr) x = if String.compare(toLower x, h) = EQUAL then r else selector hr rr x
    fun selectDb' (h : DbHandle) q = 
       case !h of NONE => raise Fail "ODBC Driver: Abuse, session closed"
                | SOME r => 
        let
          val sql = Quot.toString q
          val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_SELECT")
                  of SOME true => ignore (log sql)
                   | _ => () 
          val res : int = (log "DBODBCExecuteSQL" ; prim("@:",("DBODBCExecuteSQL",r, sql:string, getReqRec())))
            (*  val msg = Quot.toString (`selectDb: SQL Error on '` ^^ q ^^ `'`) *)
          in case res of 1 => (*DBData*) 
                             (let val res2 : string list = (log "apsmlODBCGetCNames" ;prim(":", ("apsmlODBCGetCNames",r,getReqRec())))
                                  val res3 = List.map toLower (List.rev res2)
                              in ((h, selector res3, res3),res3)
                              end handle Overflow => 
                                     raise Fail "ODBC Driver: selectDb.Database connection failed")
                       | 2 => raise Fail "ODBC Driver: selectDb: SQL was not a select statement"
                       | _ => raise Fail "ODBC Driver: selectDb: An error occured"
          end
    val toOption = List.rev o (List.map (fn [] => NONE | l as (_::_) => SOME(String.concat (List.rev l))))

    fun getRowListDb (h,f,l) = case !h of NONE => raise Fail "ODBC Driver: Abuse, session is closed"
                                        | SOME r => 
                             let val (res,res2) : ((string list) list * int) = (log "apsmlODBCGetRow" ; prim(":", ("apsmlODBCGetRow",r,getReqRec())))
                             in 
                             case res2 of 1 => SOME (toOption res)
                                        | 3 => NONE
                                        | i => raise Fail ("ODBC Driver: getRowListDb.Database connection failed with error: " ^ (Int.toString i)) 
                             end 

    fun getRowListDb2 (h,f,l) = (l,getRowListDb(h,f,l))

    fun selectDb x y = (selectDb' x y) handle Fail z => (log z; raise Fail z)
    val getRowDb = fn (h,f,l) => (Option.map f (getRowListDb (h,f,l))) handle Fail z => (log z; raise Fail z)
                                     
    fun dmlTransDb h f = case !h of NONE => raise Fail "ODBC Driver: Abuse, session is closed"
                                  | SOME r => let val _ = log("TransStart")
                                                  val res : int = prim("@:", ("DBODBCTransStart",r : int, getReqRec()))
                                              in if res = 0 
                                                 then raise Fail "ODBC Driver: Transaction already started"
                                                 else 
                    (((f h) handle X => (log "TransRollBack"; prim("@:", ("DBODBCTransRollBack",r : int, getReqRec())) : int; raise X)) before ( 
                         case !h of NONE => raise Fail "ODBC Driver: Session closed prematurely"
                                  | SOME r2 => let val _ = log "TransCommit"
                                                   val res2 : int = prim("@:", ("DBODBCTransCommit",r : int,getReqRec()))
                                               in if res2 <> 2 (* DBDml *) 
                                                  then raise Fail "ODBC Driver: dmlTransDb.Database connection failed"
                                                  else ()
                                               end))
                                               end
                                                  
  end



functor DbOracleBackend(type conn = int
                        val getReqRec : unit -> conn
                        val log : string -> string
                        val isNull : string -> bool
                        structure Info : WEB_INFO
                        structure Dynlib : WEB_DYNLIB
                        ) :> 
                               WEB_DB_BACKEND where type 'a Type = 'a Info.Type.Type =
  struct 
    type DbHandle = int option ref
    type DbResultSet = DbHandle * (string option list -> string -> string option) * string list
    type 'a Type = 'a Info.Type.Type
    structure DbBasic = NsDbBasicOra
    val first = ref true
    fun config (i:int) (t : 'a Info.Type.Type, d : string, v : 'a) : unit = 
                           let fun handRes (res : int) = case res 
                                                     of 0 => ()
                                                      | 1 => raise Fail "Oracle Driver: Unknown option"
                                                      | 2 => raise Fail "Oracle Driver: Out of memory"
                                                      | 3 => raise Fail "Oracle Driver: MaximumNumberOfConnections must be larger than SessionMaxDepth" 
                                                      | _ => raise Fail "Oracle Driver: Unknown problem in oracle driver"

                           
                               fun setInt (p : int) : unit = 
                                     if #name t = #name Info.Type.Int
                                     then 
                                     let 
                                       val res : int = 
                                        prim("@:", ("apsmlORASetVal",i,getReqRec(),p,
                                                (#from_string Info.Type.Int (#to_string t v))))
                                       in handRes res
                                       end
                                     else raise Domain
                               fun setString (p : int) : unit = 
                                     if #name t = #name Info.Type.String
                                     then 
                                       let val res : int = prim("@:", ("apsmlORASetVal",i,getReqRec(),p,
                                                    (#from_string Info.Type.String (#to_string t v))))
                                       in handRes res
                                       end
                                     else raise Domain
                               fun setBool (p : int) : unit = 
                                     if #name t = #name Info.Type.Bool
                                     then 
                                       let val res : int = prim("@:", ("apsmlORASetVal",i,getReqRec(),p,
                                                    if (#from_string Info.Type.Bool (#to_string t v))
                                                    then 1 
                                                    else 0))
                                       in handRes res
                                       end
                                     else raise Domain
                           in ((
                               if !first then 
                               (let 
                                   val b = Dynlib.dlopen (SOME "libsmloracle.so", Dynlib.NOW, false)
                                   val _ = 
                                          (log "Opened libsmloracle.so, performing resolvation" ;
                                           Dynlib.dlsym("apsmlORAGetSession","apsmlORAGetSession",b);
                                           Dynlib.dlsym("apsmlORADropSession", "apsmlORADropSession",b);
                                           Dynlib.dlsym("DBORAExecuteSQL", "DBORAExecuteSQL",b);
                                           Dynlib.dlsym("apsmlORAGetCNames", "apsmlORAGetCNames",b);
                                           Dynlib.dlsym("apsmlORAGetRow","apsmlORAGetRow",b);
                                           Dynlib.dlsym("DBORATransStart", "DBORATransStart", b);
                                           Dynlib.dlsym("DBORATransCommit", "DBORATransCommit",b);
                                           Dynlib.dlsym("DBORATransRollBack","DBORATransRollBack",b);
                                           Dynlib.dlsym("apsmlORASetVal","apsmlORASetVal",b);
                                           log "resolvation done")
                               in first:=false
                               end) handle Fail x => (log x; raise Fail x)
                               else ()
                              ) ;
                           case d 
                           of "LazyConnect" => setBool 1
                            | "UserName" => setString 2
                            | "PassWord" => setString 3
                            | "TNSname"  => setString 4
                            | "SessionMaxDepth" => setInt 5
                            | "MinimumNumberOfConnections" => setInt 6
                            | "MaximumNumberOfConnections" => setInt 7
                            | "DebugLevel" => setInt 8
                            | _ => (log "Unknown setting :" ^ d; raise Domain))
                           end
    fun getHandle (i:int) : DbHandle = let val res : int = prim(":", ("apsmlORAGetSession",i,getReqRec()))
                                 in if res = 0
                                    then raise Fail "Oracle Driver: Could not get database session"
                                    else (if res = 1 
                                    then raise Fail "Oracle Driver: Maximum nesting of oracle connections reached"
                                    else ref (SOME res))
                                 end
    fun putHandle (h:DbHandle) : unit = 
               case !h of NONE => ()
                        | SOME  r => (prim(":", ("apsmlORADropSession",r : int,getReqRec()));
                                      h:= NONE)
    fun dmlDb (h : DbHandle) q : unit = 
       case !h of NONE => raise Fail "Oracle Driver: Abuse, session closed"
                | SOME r => let 
                              val sql = Quot.toString q
                              val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_DML")
                                      of SOME true => ignore (log sql)
                                       | _ => () 
                              val  res : int = prim("@:", ("DBORAExecuteSQL",r : int, sql : string, getReqRec()))
                            in if res <> 2
                               then (* not DBDml *) 
                                  raise Fail ("Oracle Driver: dml: " ^ sql ^ " failed")
                               else ()
                            end
    fun execDb (h : DbHandle) q : unit = 
       case !h of NONE => raise Fail "Oracle Driver: Abuse, session closed"
                | SOME r => let 
                              val sql = Quot.toString q : string
                              val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_EXEC")
                                      of SOME true => ignore (log sql)
                                       | _ => () 
                              val res : int = prim("@:", ("DBORAExecuteSQL",r,sql : string, getReqRec()))
                            in if res = 0
                               then (* DBError *) 
                                  raise Fail ("Oracle Driver: exec: " ^ sql ^ " failed")
                               else ()
                            end
    val toLower = String.map Char.toLower
    fun selector ([] : string list)  ([] : string option list)  (x : string) = NONE : string option
      | selector [] (r::rr) x = raise Fail "Oracle Driver: Oracle driver corruption"
      | selector (h::hr) [] x = raise Fail "Oracle Driver: Oracle driver corruption" 
      | selector (h::hr) (r::rr) x = if String.compare(toLower x, h) = EQUAL then r else selector hr rr x
    fun selectDb (h : DbHandle) q = 
       case !h of NONE => raise Fail "Oracle Driver: Abuse, session closed"
                | SOME r => 
        let 
          val sql = Quot.toString q : string
          val _ = case Info.configGetValue (Info.Type.Bool, "DATABASE_PRINT_SELECT")
                  of SOME true => ignore (log sql)
                   | _ => () 
          val res : int = prim("@:",("DBORAExecuteSQL",r,sql : string, getReqRec()))
(*          val msg = Quot.toString (`selectDb: SQL Error on '` ^^ q ^^ `'`) *)
          in case res of 1 => (*DBData*) 
                             (let val res2 : string list = prim(":", ("apsmlORAGetCNames",r,getReqRec()))
                                  val res3 = List.map toLower (List.rev res2)
                              in ((h, selector res3, res3),res3)
                              end handle Overflow => 
                                     raise Fail "Oracle Driver: selectDb.Database connection failed")
                       | 2 => raise Fail "Oracle Driver: selectDb: SQL was not a select statement"
                       | _ => raise Fail "Oracle Driver: selectDb: An error occured"
          end
    val toOption = List.map (fn (s,i) => case i of 
                        ~2 => raise Fail "Oracle Driver: getRowListDb.Data has been truncated"
                      | ~1 => NONE
                      |  0 => if isNull s then NONE else SOME s
                      |  x => if x>0 then raise Fail ("Oracle Driver: getRowListDb. Data has been truncated, it was "
                                                      ^ (Int.toString x) ^ " bytes long")
                              else raise Fail "Oracle Driver: getRowListDb.Data Error")

    fun getRowListDb (h,f,l) = case !h of NONE => raise Fail "Oracle Driver: Session is closed"
                                        | SOME r => 
                             let val (res,res2) : ((string * int) list * int) = prim(":", ("apsmlORAGetRow",r,getReqRec()))
                             in 
                             case res2 of 1 => SOME (toOption res)
                                        | 3 => NONE
                                        | i => raise Fail ("Oracle Driver: getRowListDb.Database connection failed with error: " ^ (Int.toString i)) 
                             end 

    fun getRowListDb2 (h,f,l) = (l,getRowListDb(h,f,l))

    val getRowDb = fn (h,f,l) => Option.map f (getRowListDb (h,f,l))
                                     
    fun dmlTransDb h f = case !h of NONE => raise Fail "Oracle Driver: Abuse, session is closed"
                                  | SOME r => let 
                                                  val res : int = prim("@:", ("DBORATransStart",r : int))
                                              in if res = 0 
                                                 then raise Fail "Oracle Driver: Transaction already started"
                                                 else 
                    (((f h) handle X => (prim("@:", ("DBORATransRollBack",r : int, getReqRec())) : int; raise X)) before ( 
                         case !h of NONE => raise Fail "Oracle Driver: Session closed prematurely"
                                  | SOME r2 => let
                                                   val res2 : int = prim("@:", ("DBORATransCommit",r : int,getReqRec()))
                                               in if res2 <> 2 (* DBDml *) 
                                                  then raise Fail "Oracle Driver: dmlTransDb.Database connection failed"
                                                  else ()
                                               end))
                                               end
                                                  
  end


signature WEB_DB_UNIQUE = 
  sig 
    val unique : unit -> int
  end

structure Unique :> WEB_DB_UNIQUE =
  struct 
    val i = ref 1
    fun unique () = !i before i := (!i)+1
  end

