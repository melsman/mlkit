
functor DbFunctor (structure DbBasic : NS_DB_BASIC
		   structure Set : NS_SET
		   structure Info : NS_INFO) : NS_DB =
  struct
    type ns_db = int
    type set = Set.set
    type status = NsBasics.status
    type quot = string frag list
    fun quotToString (q : quot) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) q)

    structure Pool : NS_POOL =
      struct
	type pool = string
	exception DbPoolError of string

	local
	  val pools : pool list ref = ref [] 
	in
	  fun initPools (sectionName,key) =
	    case Info.configGetValue{sectionName=sectionName, key=key} of
	      SOME ps => pools := (String.tokens (fn ch => ch = #",") ps)
	    | NONE => raise (DbPoolError ("No pools specified in file " ^ Info.configFile() ^ 
			     "; section " ^ sectionName ^ " and key " ^ key ^ "."))
	  fun initPoolsL pns = pools := pns
	  fun putPool pn = pools := pn :: !pools
	  fun getPool () = 
	    case !pools of
	      [] => raise DbPoolError "In getPool: No more pools"
	    | pn::ps => (pools := ps; pn)
	  fun toList () = !pools
	  fun pp () = 
	    let
	      fun sl2s sep [] = ""
  	        | sl2s sep l = concat (tl (foldr (fn (s,acc)=>sep::s::acc) [] l))
	    in
	      sl2s "," (!pools)
	    end
	end
      end
    type pool = Pool.pool
    type db = pool * ns_db

    open DbBasic

    fun init(sectionName, key) = Pool.initPools(sectionName, key)

    fun poolGetHandle (pool : pool) : db =
      (pool, prim("nssml_DbPoolGetHandle", "nssml_DbPoolGetHandle", pool))
    fun poolPutHandle (db : db) : unit =
      prim("nssml_DbPoolPutHandle", "nssml_DbPoolPutHandle", #2 db)
    fun dmlDb (db : db, q: quot) : status =
      prim("nssml_DbDML", "nssml_DbDML", (#2 db, quotToString q))
    fun panicDmlDb (db:db) (f_panic: quot -> 'a) (q: quot) : unit =
      (dmlDb (db,q); () handle X => (f_panic (q ^^ `^("\n") ^(General.exnMessage X)`); ()))

    fun dmlTransDb (db : db, f : db -> 'a) : 'a =
      let
	val _ = dmlDb (db,DbBasic.beginTrans)
	val res = f db;
      in
	dmlDb (db,DbBasic.endTrans);
	res
      end handle X => (dmlDb (db,DbBasic.roolback); raise X)
    fun panicDmlTransDb (db:db) (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
      dmlTransDb (db,f) handle X => (f_panic(`^(General.exnMessage X)`))

    fun getHandle () : db = poolGetHandle(Pool.getPool())
    fun putHandle db : unit = (poolPutHandle db; Pool.putPool (#1 db))
      
    fun dml (q: quot) : status =
      let val db = getHandle()
      in (dmlDb (db,q) before putHandle db)
	handle X => (putHandle db; raise X)
      end
    fun maybeDml (q: quot) : unit = ((dml q; ()) handle X => ())
    fun panicDml (f_panic: quot -> 'a) (q: quot) : unit =
      ((dml q; ()) handle X => (f_panic (q ^^ `^("\n") ^(General.exnMessage X)`); ()))
    fun errorDml (f_error: unit -> 'a) (q: quot) : unit =
      ((dml q; ()) handle X => (f_error(); ()))
    fun dmlTrans (f: db -> 'a) : 'a =
      let 
	val db = getHandle()
      in
	let
	  val res = dmlTransDb (db,f)
	in 
	  putHandle db;
	  res
	end handle X => (putHandle db; raise X)
      end
    fun panicDmlTrans (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
      dmlTrans f handle X => (f_panic(`^(General.exnMessage X)`))

    fun select (db: db, q: quot) : Set.set =
      prim("nssml_DbSelect", "nssml_DbSelect", (#2 db, quotToString q))
    fun getRow (db : db, s : Set.set) : status =
      prim("nssml_DbGetRow", "nssml_DbGetRow", (#2 db, s))

    fun foldDb (db:db, f:(string->string)*'a->'a, acc:'a, sql:quot) : 'a =
      let val s : Set.set = select(db, sql)
	fun g n = Set.getOpt(s, n, "##")
	fun loop (acc:'a) : 'a =
	  if (getRow(db,s) <> NsBasics.END_DATA) then loop (f(g,acc))
	  else acc
      in loop acc
      end

    fun listDb (db:db, f:(string->string)->'a, sql: quot) : 'a list = 
      let 
	val s : Set.set = select(db, sql)
	fun g n = Set.getOpt(s, n, "##")
	fun loop () : 'a list =
	  if (getRow(db,s) <> NsBasics.END_DATA) then f g :: loop()
	  else []
      in 
	loop ()
      end

    fun wrapDb f =
      let val db = getHandle()
      in (f db before putHandle db)
	handle X => (putHandle db; raise X)
      end
      
    fun fold (f:(string->string)*'a->'a, acc:'a, sql:quot) : 'a =
      wrapDb (fn db => foldDb (db,f,acc,sql))

    fun list (f:(string->string)->'a, sql:quot) : 'a list = wrapDb (fn db => listDb(db,f,sql))

    fun oneRow' (f:(string->string)->'a, sql:quot) : 'a = 
      case list (f,sql) of
	[] => raise Fail "No rows"
      | [r] => r
      | _ => raise Fail "More than one row"

    fun zeroOrOneRow' (f:(string->string)->'a, sql:quot) : 'a option = 
      case list (f,sql) of
	[] => NONE
      | [r] => SOME r
      | _ => raise Fail "More than one row"

    fun oneFieldDb(db,sql) : string =
      let val s : Set.set = select(db, sql)
      in 
	if getRow(db,s) <> NsBasics.END_DATA then
	  if Set.size s = 1 then 
	    case Set.value(s,0) of
	      SOME s => s
	    | NONE => raise Fail "Db.oneFieldDb.no value in set"
	  else raise Fail "Db.oneFieldDb.size of set not one"
	else raise Fail "Db.oneFieldDb.no rows"
      end

    fun oneField (sql : quot) : string = 
      wrapDb (fn db => oneFieldDb(db,sql))

    fun zeroOrOneFieldDb(db,sql) : string option =
      let val s : Set.set = select(db, sql)
      in 
	if getRow(db,s) <> NsBasics.END_DATA then
	  if Set.size s = 1 then 
	    Set.value(s,0)
	  else NONE
	else NONE
      end

    fun zeroOrOneField (sql : quot) : string option =
      wrapDb (fn db => zeroOrOneFieldDb(db,sql))

    fun oneRowDb(db,sql) : string list =
      let val s : Set.set = select(db, sql)
      in 
	if getRow(db,s) <> NsBasics.END_DATA then
	  Set.foldr (fn ((k,v), a) => v :: a) nil s
	else raise Fail "Db.oneRowDb.no rows"
      end

    fun oneRow sql : string list =
      wrapDb (fn db => oneRowDb(db,sql))

    fun zeroOrOneRowDb(db,sql) : string list option =
      let val s : Set.set = select(db, sql)
      in 
	if getRow(db,s) <> NsBasics.END_DATA then
	  SOME(Set.foldr (fn ((k,v), a) => v :: a) nil s)
	else NONE
      end

    fun zeroOrOneRow sql : string list option =
      wrapDb (fn db => zeroOrOneRowDb(db,sql))

    fun existsOneRowDb(db,sql) : bool =
      let val s : Set.set = select(db, sql)
      in 
	if getRow(db,s) <> NsBasics.END_DATA then true else false
      end

    fun existsOneRow sql : bool =
      wrapDb (fn db => existsOneRowDb(db,sql))

    fun qq s =
      let 
	fun qq_s' [] = []
	  | qq_s' (x::xs) = if x = #"'" then x :: x :: (qq_s' xs) else x :: (qq_s' xs)
      in
	implode(qq_s'(explode s))
      end

    fun qq' s = concat ["'", qq s, "'"]

    fun toDate s = 
      let
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
	(case (RegExp.extract o RegExp.fromString) "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])" s of
	   SOME [yyyy,mm,dd] => SOME (Date.date{year=Option.valOf (Int.fromString yyyy),
						month=mthToName (Option.valOf (Int.fromString mm)),
						day=Option.valOf (Int.fromString dd),
						hour=0,minute=0,second=0,offset=NONE})
	 | _ => NONE)
      end
    handle _ => NONE

    fun valueList vs = String.concatWith "," (List.map qq' vs)
    fun setList vs = String.concatWith "," (List.map (fn (n,v) => n ^ "=" ^ qq' v) vs)

    fun seqNextval (seqName:string) : int = 
      let val s = oneField `select ^(seqNextvalExp seqName) ^fromDual`
      in case Int.fromString s of
	SOME i => i
      | NONE => raise Fail "Db.seqNextval.nextval not an integer"	
      end
  end

structure NsDbBasicOra : NS_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = seq_name ^ ".nextval"
    val fromDual = "from dual"
    val sysdateExp = "sysdate"
    val beginTrans = `begin transaction`
    val endTrans = `commit`
    val roolback = `end transaction`
  end

structure NsDbBasicPG : NS_DB_BASIC =
  struct
    fun seqNextvalExp seq_name = "nextval('" ^ seq_name ^ "')"
    val fromDual = ""
    val sysdateExp = "now()"
    val beginTrans = `begin`
    val endTrans = `commit`
    val roolback = `roolback`
  end

