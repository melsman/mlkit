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

functor DbFunctor (structure DbBasic : NS_DB_BASIC) : NS_DB =
  struct
    type ns_db = int
    type set = NsSet.set
    type status = NsBasics.status
    type quot = string frag list
    fun quotToString (q : quot) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) q)

    structure Pool =
      struct
	type pool = string
	type db = pool * ns_db
	local
	  val pools : pool list ref = ref [] 
	in
(*
	  fun initPools {section,key} =
	    case NsInfo.configGetValue{sectionName=section, key=key} of
	      SOME ps => pools := (String.tokens (fn ch => ch = #",") ps)
	    | NONE => raise (Fail ("initPools.no pools specified in file " ^ NsInfo.configFile() ^ 
			     "; section " ^ sectionName ^ " and key " ^ key ^ "."))
*)
	  fun initPools pns = pools := pns
	  fun putPool pn = pools := pn :: !pools
	  fun getPool () = 
	    case !pools of
	      [] => raise Fail "getPool.no more pools"
	    | pn::ps => (pools := ps; pn)
	  fun toList () = !pools
	  fun pp () = 
	    let
	      fun sl2s sep [] = ""
  	        | sl2s sep l = concat (tl (foldr (fn (s,acc)=>sep::s::acc) [] l))
	    in
	      sl2s "," (!pools)
	    end
	  fun ppDb (db:db) : string = Quot.toString `db=(^(#1 db),^(Int.toString (#2 db)))`
	end

	fun poolGetHandle (pool : pool) : db =
	  let
	    val h : ns_db = prim("@Ns_DbPoolGetHandle", pool)

val _ = NsDebug.addMsg `*******get handle ^(ppDb (pool,h))`

	  in
	    if h = 0 then raise Fail "poolGetHandle:Can't allocate handle" else (pool,h)
	  end

	fun poolPutHandle (db : db) : unit = 
let

val _ = NsDebug.addMsg `*******put handle ^(ppDb db)`
in
	  prim("@Ns_DbPoolPutHandle", #2 db) end
      end

    type pool = Pool.pool
    type db = Pool.db

    open DbBasic

    structure Handle : NS_DB_HANDLE =
      struct
	structure Pool = Pool
	type db = Pool.db
	type set = NsSet.set

	fun getHandle () : db = Pool.poolGetHandle(Pool.getPool())

	fun putHandle db : unit = (Pool.poolPutHandle db; Pool.putPool (#1 db))

	val initPools = Pool.initPools

	fun wrapDb f =
	  let val db = getHandle()
val _ = NsDebug.addMsg `*******db=(^(#1 db),^(Int.toString (#2 db)))`
	  in (f db before putHandle db)
	    handle X => (putHandle db; raise X)
	  end
      
	fun dmlDb (db : db) (q: quot) : unit =
	  let
	    val status = prim("@Ns_DbDML", (#2 db, quotToString q))
	  in
	    if status = NsBasics.ERROR then 
	      raise Fail ("dml: " ^ Quot.toString q ^ " failed") 
	    else ()
	  end

	fun execDb (db : db) (q: quot) : unit =
	  let
	    val status = prim("@Ns_DbExec", (#2 db, quotToString q))
	  in
	    if status = NsBasics.ERROR then 
	      raise Fail ("exec: " ^ Quot.toString q ^ " failed") 
	    else ()
	  end

	fun panicDmlDb (db:db) (f_panic: quot -> 'a) (q: quot) : unit =
	  (dmlDb db q handle X => (f_panic (q ^^ `^("\n") ^(General.exnMessage X)`); ()))

	fun dmlTransDb (db : db) (f : db -> 'a) : 'a =
	  let
	    val _ = dmlDb db DbBasic.beginTrans
	    val res = f db;
	  in
	    dmlDb db DbBasic.endTrans;
	    res
	  end handle X => (dmlDb db DbBasic.rollback; raise X)

	fun dmlTrans (f: db -> 'a) : 'a =
	  let 
	    val db = getHandle()
	  in
	    let
	      val res = dmlTransDb db f
	    in 
	      putHandle db;
	      res
	    end handle X => (putHandle db; raise X)
	  end
    
	fun panicDmlTransDb (db:db) (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
	  dmlTransDb db f handle X => (f_panic(`^(General.exnMessage X)`))

	fun panicDmlTrans (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
	  dmlTrans f handle X => (f_panic(`^(General.exnMessage X)`))

	fun selectDb (db: db, q: quot) : set =
	  let 
val _ = NsDebug.addMsg `selectDb,db=(^(#1 db),^(Int.toString (#2 db)))`
val _ = NsDebug.addMsg (`selectDb, SQL:` ^^ q)
	    fun isNull(s : set) : bool = prim("__is_null",s)
	    val res = prim("@Ns_DbSelect", (#2 db, quotToString q))
	  in 
	    if isNull res 
	      then  
		let 
		  val msg = Quot.toString (`selectDb: SQL Error on '` ^^ q ^^ `'`)
		in 
		  raise Fail msg
		end
	    else res
	  end

	fun getRowDb (db : db, s : set) : status =
	  prim("@Ns_DbGetRow", (#2 db, s))

	fun foldDb (db:db) (f:(string->string)*'a->'a) (acc:'a) (sql:quot) : 'a =
	  let 
	    val s : set = selectDb(db, sql)
	    fun g n = NsSet.getOpt(s, n, "##")
	    fun loop (acc:'a) : 'a =
	      if (getRowDb(db,s) <> NsBasics.END_DATA) then loop (f(g,acc))
	      else acc
	  in loop acc
	  end

	fun foldSetDb (db:db) (f:set*'a->'a) (acc:'a) (sql:quot) : 'a =
	  let 
	    val s : set = selectDb(db, sql)
	    fun loop (acc:'a) : 'a =
	      if (getRowDb(db,s) <> NsBasics.END_DATA) then loop (f(s,acc))
	      else acc
	  in loop acc
	  end

	fun appDb (db:db) (f:(string->string)->'a) (sql:quot) : unit =
	  let 
	    val s : set = selectDb(db, sql)
	    fun g n = NsSet.getOpt(s, n, "##")
	    fun loop () : unit =
	      if (getRowDb(db,s) <> NsBasics.END_DATA) then (f g; loop ())
	      else ()
	  in loop ()
	  end

	fun listDb (db:db) (f:(string->string)->'a) (sql: quot) : 'a list = 
	  let 
	    val s : set = selectDb(db, sql)
	    fun g n = NsSet.getOpt(s, n, "##")
	    fun loop () : 'a list =
	      if (getRowDb(db,s) <> NsBasics.END_DATA) then f g :: loop()
	      else []
	  in 
	    loop ()
	  end

	fun oneFieldDb db sql : string =
	  let 
	    val s : set = selectDb(db, sql)
	    val res =
	      if getRowDb(db,s) <> NsBasics.END_DATA then
		if NsSet.size s = 1 then 
		  case NsSet.value(s,0) of
		    SOME s => s
		  | NONE => raise Fail "Db.oneFieldDb.no value in set"
		else raise Fail "Db.oneFieldDb.size of set not one"
	      else raise Fail "Db.oneFieldDb.no rows"
	  in
	    if getRowDb(db,s) = NsBasics.END_DATA then res 
	    else raise Fail "oneFieldDb.more than one row"
	  end

	fun zeroOrOneFieldDb db sql : string option =
	  let 
	    val s : set = selectDb(db, sql)
	  in
	    if getRowDb(db,s) <> NsBasics.END_DATA then
	      let 
		val res = 
		  if NsSet.size s = 1 then 
		    NsSet.value(s,0)
		  else raise Fail "zeroOrOneFieldDb.size of set is not one"
	      in
		if getRowDb(db,s) = NsBasics.END_DATA then
		  res 
		else raise Fail "zeroOrOneFieldDb.more than one row"
	      end
	    else NONE (* OK, no rows *)
	  end
	
	fun oneRowDb db sql  : string list =
	  let 
	    val s : set = selectDb(db, sql)
	    val res =
	      if getRowDb(db,s) <> NsBasics.END_DATA then
		NsSet.foldr (fn ((k,v), a) => v :: a) nil s
	      else raise Fail "Db.oneRowDb.no rows"
	  in
	    if getRowDb(db,s) = NsBasics.END_DATA then
	      res
	    else raise Fail "oneRowDb.more that one row"
	  end
	
	fun oneRowDb' db (f:(string->string)->'a) (sql:quot) : 'a =
	  let 
	    val s : set = selectDb(db, sql)
	    fun g n = NsSet.getOpt(s, n, "##")
	    val res =
	      if getRowDb(db,s) <> NsBasics.END_DATA then
		f g
	      else raise Fail "Db.oneRowDb'.no rows"
	  in
	    if getRowDb(db,s) = NsBasics.END_DATA then
	      res
	    else raise Fail "oneRowDb'.more that one row"
	  end
	
	fun zeroOrOneRowDb db sql : string list option =
	  let 
	    val s : set = selectDb(db, sql)
	  in
	    if getRowDb(db,s) <> NsBasics.END_DATA then
	      let 
		val res = SOME (NsSet.foldr (fn ((k,v), a) => v :: a) nil s)
	      in
		if getRowDb(db,s) = NsBasics.END_DATA then
		  res
		else raise Fail "zeroOrOneRowDb.more than one row"
	      end
	    else NONE (* Ok, no rows *)
	  end

	fun zeroOrOneRowDb' db f sql : 'a option =
	  let 
	    val s : set = selectDb(db, sql)
	    fun g n = NsSet.getOpt(s, n, "##")
	  in
	    if getRowDb(db,s) <> NsBasics.END_DATA then
	      let 
		val res = SOME (f g)
	      in
		if getRowDb(db,s) = NsBasics.END_DATA then
		  res
		else raise Fail "zeroOrOneRowDb'.more than one row"
	      end
	    else NONE (* Ok, no rows *)
	  end

	fun existsOneRowDb db sql : bool =
	  let val s : set = selectDb(db, sql)
	  in if getRowDb(db,s) <> NsBasics.END_DATA then true 
	     else false
	  end

	fun seqNextvalDb db (seqName:string) : int = 
	  let val s = oneFieldDb db `select ^(seqNextvalExp seqName) ^fromDual`
	  in case Int.fromString s of
	    SOME i => i
	  | NONE => raise Fail "Db.seqNextval.nextval not an integer"	
	  end

	fun seqCurrvalDb db (seqName:string) : int = 
	  let val s = oneFieldDb db `select ^(seqCurrvalExp seqName) ^fromDual`
	  in case Int.fromString s of
	    SOME i => i
	  | NONE => raise Fail "Db.seqCurrval.nextval not an integer"	
	  end

       (* Stored Procedures *)
       fun execSpDb (db: db) ([]: quot list) : unit = ()
         | execSpDb (db: db) (qs: quot list) : unit =
	 let
	   val body = Quot.concatWith ";\n"  qs
	 in
	   dmlDb db (`declare begin ` ^^ body ^^ `; end;`)
         end
    end (* structure Handle *)

    fun dml (q: quot) : unit = Handle.wrapDb (fn db => Handle.dmlDb db q)
    fun exec (q: quot) : unit = Handle.wrapDb (fn db => Handle.execDb db q)    
    
    fun maybeDml (q: quot) : unit = dml q handle X => ()

    fun panicDml (f_panic: quot -> 'a) (q: quot) : unit =
      dml q handle X => (f_panic (q ^^ `^("\n") ^(General.exnMessage X)`); ())

    (* Stored Procedures *)
    fun execSp qs : unit = Handle.wrapDb (fn db => Handle.execSpDb db qs)

    fun fold (f:(string->string)*'a->'a) (acc:'a) (sql:quot) : 'a =
      Handle.wrapDb (fn db => Handle.foldDb db f acc sql)

    fun foldSet (f:set*'a -> 'a) (acc:'a) (sql:quot) : 'a =
      Handle.wrapDb (fn db => Handle.foldSetDb db f acc sql)

    fun app (f:(string->string)->'a) (sql:quot) : unit =
      Handle.wrapDb (fn db => Handle.appDb db f sql)

    fun list (f:(string->string)->'a) (sql:quot) : 'a list = 
      Handle.wrapDb (fn db => Handle.listDb db f sql)

    fun oneField (sql : quot) : string = 
      Handle.wrapDb (fn db => Handle.oneFieldDb db sql)

    fun zeroOrOneField (sql : quot) : string option =
      Handle.wrapDb (fn db => Handle.zeroOrOneFieldDb db sql)

    fun oneRow sql : string list =
      Handle.wrapDb (fn db => Handle.oneRowDb db sql)

    fun oneRow' (f:(string->string)->'a) (sql:quot) : 'a = 
      Handle.wrapDb (fn db => Handle.oneRowDb' db f sql)

    fun zeroOrOneRow sql : string list option =
      Handle.wrapDb (fn db => Handle.zeroOrOneRowDb db sql)

    fun zeroOrOneRow' (f:(string->string)->'a) (sql:quot) : 'a option = 
      Handle.wrapDb (fn db => Handle.zeroOrOneRowDb' db f sql)

    fun existsOneRow sql : bool =
      Handle.wrapDb (fn db => Handle.existsOneRowDb db sql)

    fun qq s =
      let 
	fun qq_s' [] = []
	  | qq_s' (x::xs) = if x = #"'" then x :: x :: (qq_s' xs) else x :: (qq_s' xs)
      in
	implode(qq_s'(explode s))
      end

    fun qqq s = concat ["'", qq s, "'"]

    local
      fun mthToName mth =
	case mth of
	  1 => Date.Jan
	| 2 => Date.Feb
	| 3 => Date.Mar
	| 4 => Date.Apr
	| 5 => Date.May
	| 6 => Date.Jun
	| 7 => Date.Jul
	| 8 => Date.Aug
	| 9 => Date.Sep
	| 10 => Date.Oct
	| 11 => Date.Nov
	| 12 => Date.Dec
	| _ => raise Fail ("DbFunctor.toDate: " ^ (Int.toString mth))
    in
      fun toDate s = 
	(case (RegExp.extract o RegExp.fromString) "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9]).*" s of
	   SOME [yyyy,mm,dd] => SOME (Date.date{year=Option.valOf (Int.fromString yyyy),
						month=mthToName (Option.valOf (Int.fromString mm)),
						day=Option.valOf (Int.fromString dd),
						hour=0,minute=0,second=0,offset=NONE})
	 | _ => NONE)
	   handle _ => NONE

      fun toTimestamp t =
	(case (RegExp.extract o RegExp.fromString) "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9]) ([0-9][0-9]):([0-9][0-9]):([0-9][0-9]).*" t of
	   SOME [yyyy,mm,dd,h,m,s] => SOME (Date.date{year=Option.valOf (Int.fromString yyyy),
						      month=mthToName (Option.valOf (Int.fromString mm)),
						      day=Option.valOf (Int.fromString dd),
						      hour=Option.valOf(Int.fromString h),
						      minute=Option.valOf(Int.fromString m),
						      second=Option.valOf(Int.fromString s),
						      offset=NONE})
	 | _ => NONE)
	   handle _ => NONE
    end

    fun toBool "t" = SOME true
      | toBool "f" = SOME false
      | toBool _ = NONE

    fun fromBool true = "t"
      | fromBool false = "f"

    fun toReal r = Real.fromString r
    fun fromReal r = Real.toString r

    fun valueList vs = String.concatWith "," (List.map qqq vs)
    fun setList vs = String.concatWith "," (List.map (fn (n,v) => n ^ "=" ^ qqq v) vs)

    fun seqNextval (seqName:string) : int = 
      Handle.wrapDb (fn db => Handle.seqNextvalDb db seqName)

    fun seqCurrval (seqName:string) : int = 
      Handle.wrapDb (fn db => Handle.seqCurrvalDb db seqName)
  end

structure NsDbBasicOra : NS_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = seq_name ^ ".nextval"
    fun seqCurrvalExp seq_name = seq_name ^ ".currval"
    val fromDual = "from dual"
    val sysdateExp = "sysdate"
    val beginTrans = `begin transaction`
    val endTrans = `end transaction`
    val rollback = `rollback`
    fun fromDate d = "to_date('" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "','YYYY-MM-DD HH24:MI:SS')"
    fun toTimestampExp d = "to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"
    val timestampType = "date"
  end

structure NsDbBasicPG : NS_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = "nextval('" ^ seq_name ^ "')"
    fun seqCurrvalExp seq_name = "currval('" ^ seq_name ^ "')"
    val fromDual = ""
    val sysdateExp = "now()"
    val beginTrans = `begin`
    val endTrans = `commit`
    val rollback = `rollback`
    fun fromDate d = "'" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "'"
    fun toTimestampExp d = "to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"
    val timestampType = "datetime"
  end

structure NsDbBasicMySQL : NS_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = "null"
    fun seqCurrvalExp seq_name = raise Fail "seqCurrvalExp not supported on MySQL"
    val fromDual = ""
    val sysdateExp = "now()"
    val beginTrans = `begin`
    val endTrans = `commit`
    val rollback = `rollback`
    fun fromDate d = "'" ^ (Date.fmt "%Y-%m-%d %H:%M:%S" d) ^ "'"
    fun toTimestampExp d = d (*"to_char(" ^ d ^ ",'YYYY-MM-DD HH24:MI:SS')"*)
    val timestampType = "datetime"
  end

