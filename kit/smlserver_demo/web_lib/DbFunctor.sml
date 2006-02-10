
functor DbFunctor (structure DbBackend : WEB_DB_BACKEND
                   val name : string)
                  
                    :> WEB_DB where type 'a Type = 'a DbBackend.Type = 
  struct
    structure DbBasic = DbBackend.DbBasic
    type ns_db = int
    type quot = string frag list
    type 'a Type = 'a DbBackend.Type
    val dbCache = Web.Cache.get(Web.Cache.String, Web.Cache.Int,
                                "__InternalCache:DBTABLE",
                                Web.Cache.WhileUsed(NONE,NONE))
    val unique_number = case (Web.Cache.lookup dbCache name)
                        of NONE => (let val u = Unique.unique()
                                    in (Web.Cache.insert (dbCache, name, u, NONE); u)
                                    end)
                         | SOME u => u
    val config = fn a =>  DbBackend.config unique_number a
    fun quotToString (q : quot) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) q)

    (*structure Pool =
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
  end*)

(*    type pool = Pool.pool *)
    type db = DbBackend.DbHandle

    open DbBasic

    structure Handle : WEB_DB_HANDLE =
      struct
	type db = DbBackend.DbHandle

	val getHandle = fn () => DbBackend.getHandle unique_number

	val putHandle = DbBackend.putHandle
 
	fun wrapDb f =
	  let val db = getHandle()
	  in (f db before putHandle db)
	    handle X => (putHandle db; raise X)
	  end
      
	fun panicDmlDb (db:db) (f_panic: quot -> 'a) (q: quot) : unit =
	  (DbBackend.dmlDb db q handle X => (f_panic (q ^^ `^("\n") ^(General.exnMessage X)`); ()))
 
  val dmlDb = DbBackend.dmlDb

  val execDb = DbBackend.execDb

	val dmlTransDb = DbBackend.dmlTransDb

  val dmlTrans = fn f => wrapDb (fn z => dmlTransDb z f)
    
	fun panicDmlTransDb (db:db) (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
	  dmlTransDb db f handle X => (f_panic(`^(General.exnMessage X)`))

	fun panicDmlTrans (f_panic: quot -> 'a) (f: db -> 'a) : 'a =
	  dmlTrans f handle X => (f_panic(`^(General.exnMessage X)`))

	fun foldDbCol (db:db) (f:string list -> (string->string option)*'a->'a) (acc:'a) (sql:quot) : 'a =
	  let 
	    val (s,r) = DbBackend.selectDb db sql
      val f' = f r
	    fun loop (acc:'a) : 'a =
	      case DbBackend.getRowDb s of SOME g => loop (f'(g,acc))
                                   | NONE => acc
	  in loop acc
	  end

  fun foldDb (db : db) f acc sql = foldDbCol db (fn _ => fn (g,a) => f (fn x => getOpt(g x, "##"),a)) acc sql

	fun appDbCol (db:db) (f:string list -> (string->string option)->'a) (sql:quot) : unit =
	  let 
	    val (s,r) = DbBackend.selectDb db sql
      val f' = f r
	    fun loop () : unit =
	      case DbBackend.getRowDb s of SOME g => (f' g; loop ())
                                   | NONE => ()
	  in loop ()
	  end
  
  fun appDb db f sql = appDbCol db (fn _ => fn g => f(fn x=> getOpt(g x, "##"))) sql

	fun listDbCol (db:db) (f:string list -> (string->string option)->'a) (sql: quot) : 'a list = 
	  let 
	    val (s,r) = DbBackend.selectDb db sql
      val f' = f r
	    fun loop () : 'a list =
	      case DbBackend.getRowDb s of SOME g => f' g :: loop()
                                   | NONE => []
	  in 
	    loop ()
	  end
 
  fun listDb db f sql = listDbCol db (fn _ => fn g => f(fn x=> getOpt(g x, "##"))) sql

  fun oneWrap f m db sql = let val (s,r) = DbBackend.selectDb db sql 
                             val res = f s
                         in case DbBackend.getRowDb s of NONE => res
                                                       | SOME _ => raise Fail (m ^ ".more than one row")
                         end

  val oneFieldDb = oneWrap
       (fn s => 
	      case DbBackend.getRowListDb s of NONE => raise Fail "Db.oneFieldDb.no rows"
                                       | SOME [x] => getOpt(x,"##")
                                       | SOME _ => raise Fail "Db.oneFieldDb.size of result not one")
       "oneFieldDb"

  val zeroOrOneFieldDb = oneWrap
       (fn s => case DbBackend.getRowListDb s of NONE => NONE
                                     | SOME [x] => SOME (getOpt(x,"##"))
                                     | SOME _ => raise Fail "zeroOrOneFieldDb.size of set is not one")
       "zeroOrOneFieldDb"

  val oneRowDb = oneWrap
       (fn s => case DbBackend.getRowListDb s of NONE => raise Fail "Db.oneRowDb.no rows"
                                               | SOME l => map (fn x => getOpt(x, "##")) l)
       "oneRowDb"
  
	fun oneRowDb' db (f:(string->string)->'a) (sql:quot) : 'a =
	  let 
	    val (s,_) = DbBackend.selectDb db sql
	    val res =
	      case DbBackend.getRowDb s of NONE => raise Fail "Db.oneRowDb'.no rows"
                                   | SOME g => f(fn x=> getOpt(g x,"##"))
	  in
	    case DbBackend.getRowDb s of NONE => res
                                 | SOME _ => raise Fail "oneRowDb'.more that one row"
	  end
	
  val zeroOrOneRowDb = oneWrap (fn r=> Option.map (map (fn x => getOpt(x,"##")))
                                (DbBackend.getRowListDb r)) "zeroOrOneRowDb"

	fun zeroOrOneRowDb' db f sql : 'a option =
	  let 
	    val (s,_) = DbBackend.selectDb db sql
      val res = Option.map (fn g => f(fn y => getOpt(g y, "##"))) (DbBackend.getRowDb s)
	  in
	    case DbBackend.getRowDb s of NONE => res
                                 | SOME _ => raise Fail "zeroOrOneRowDb'.more than one row"
	  end

	fun existsOneRowDb db sql : bool =
	  let val (s,_) = DbBackend.selectDb db sql
    in case DbBackend.getRowDb s of NONE => false
                                  | SOME _ => true
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

    fun foldCol (f:string list -> (string->string option)*'a->'a) (acc:'a) (sql:quot) : 'a =
      Handle.wrapDb (fn db => Handle.foldDbCol db f acc sql)

    fun app (f:(string->string)->'a) (sql:quot) : unit =
      Handle.wrapDb (fn db => Handle.appDb db f sql)

    fun appCol (f:string list -> (string->string option)->'a) (sql:quot) : unit =
      Handle.wrapDb (fn db => Handle.appDbCol db f sql)

    fun list (f:(string->string)->'a) (sql:quot) : 'a list = 
      Handle.wrapDb (fn db => Handle.listDb db f sql)

    fun listCol (f:string list -> (string->string option)->'a) (sql:quot) : 'a list = 
      Handle.wrapDb (fn db => Handle.listDbCol db f sql)

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
    fun setList vs = String.concatWith (Quot.toString `,
`) (List.map (fn (n,v) => n ^ "=" ^ qqq v) vs)

    fun seqNextval (seqName:string) : int = 
      Handle.wrapDb (fn db => Handle.seqNextvalDb db seqName)

    fun seqCurrval (seqName:string) : int = 
      Handle.wrapDb (fn db => Handle.seqCurrvalDb db seqName)
  end

