signature DB_BASIC =
  sig
    val seq_nextval_exp : string -> string
  end

structure DbBasicOra : DB_BASIC =
  struct
    fun seq_nextval_exp seq_name = seq_name ^ ".nextval"
  end

structure DbBasicPG : DB_BASIC =
  struct
    fun seq_nextval_exp seq_name = "nextval('" ^ seq_name ^ "')"
  end

signature POOL =
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

signature DB =
  sig
    include DB_BASIC

    structure Pool : POOL

    type status
    type set
    type ns_db
    type pool = Pool.pool
    type db = pool * ns_db

    val init : string * string -> unit
    val poolGetHandle : pool -> db
    val poolPutHandle : db -> unit
    val getHandle : unit -> db
    val putHandle : db -> unit
    val dmlDb : db * string -> status
    val dml : string -> status
    val select : db * string -> set
    val getRow : db * set -> status
    val foldDb : db * ((string->string)*'a->'a) * 'a * string -> 'a
    val fold : ((string->string)*'a->'a) * 'a * string -> 'a
    val qq : string -> string
  end

functor DbFunctor (structure DbBasic : DB_BASIC
		   structure Set : NS_SET
		   structure Info : NS_INFO) : DB =
  struct
    type ns_db = int
    type set = Set.set
    type status = int        (* see nsthread.h *)
    val OK = 0 and ERROR = ~1 and END_DATA = 4

    structure Pool : POOL =
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
    fun dmlDb (db : db, s: string) : status =
      prim("nssml_DbDML", "nssml_DbDML", (#2 db, s))

    fun getHandle () : db = poolGetHandle(Pool.getPool())
    fun putHandle db : unit = (poolPutHandle db; Pool.putPool (#1 db))
      
    fun dml (s: string) : status =
      let val db = getHandle()
      in (dmlDb (db,s) before putHandle db)
	handle X => (putHandle db; raise X)
      end
    fun select (db : db, s : string) : Set.set =
      prim("nssml_DbSelect", "nssml_DbSelect", (#2 db, s))
    fun getRow (db : db, s : Set.set) : status =
      prim("nssml_DbGetRow", "nssml_DbGetRow", (#2 db, s))

    fun foldDb (db:db, f:(string->string)*'a->'a, acc:'a, sql:string) : 'a =
      let val s : Set.set = select(db, sql)
	fun g n = Set.getOpt(s, n, "##")
	fun loop (acc:'a) : 'a =
	  if (getRow(db,s) <> END_DATA) then loop (f(g,acc))
	  else acc
      in loop acc
      end

    fun fold (f:(string->string)*'a->'a, acc:'a, sql:string) : 'a =
      let val db = getHandle()
      in (foldDb (db,f,acc,sql) before putHandle db)
	handle X => (putHandle db; raise X)
      end

    fun qq s =
      let 
	fun qq_s' [] = []
	  | qq_s' (x::xs) = if x = #"'" then x :: x :: (qq_s' xs) else x :: (qq_s' xs)
      in
	implode(qq_s'(explode s))
      end
  end