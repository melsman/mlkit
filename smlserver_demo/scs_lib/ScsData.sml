signature SCS_DATA =
  sig
    val getOracleId : unit -> int
    val getOracleIdTrans : Db.Handle.db -> int

    val gToInt       : (string -> string) -> string -> int
    val gToIntOpt    : (string -> string) -> string -> int option
    val gToReal      : (string -> string) -> string -> real
    val gToRealOpt   : (string -> string) -> string -> real option
    val gToBool      : (string -> string) -> string -> bool
    val gToDate      : (string -> string) -> string -> Date.date
    val gToTimestamp : (string -> string) -> string -> Date.date
    val gToStringOpt : (string -> string) -> string -> string option

    (* [mk_selectBoxFromDb sql g_fn fv v_opt] returns a select box, where 
       elements are taken from the DB.
       The g_fn sends a database g function to a pair of 
       select element (value, name) *)
    val mk_selectBoxFromDb : quot -> ((string->string) -> (string*string)) 
      -> string -> int option -> quot
      

    (* [mk_selectBoxFromDb' sql g_fn add_opts fv v_opt] is similar to 
	mk_selectBoxFromDb except that it is possible to prepend additional
	(value, name) pairs in the list add_opts *)
    val mk_selectBoxFromDb' : quot -> ((string->string) -> (string*string)) 
      -> (string*string) list -> string -> int option -> quot

    (* [mk_selectBoxFromDb'' sql g_fn pre_opts post_opts fv v_opt] 
	is similar to mk_selectBoxFromDb' except that it is also possible 
	to append additional
	(value, name) pairs in the list post_opts *)
    val mk_selectBoxFromDb'' : quot -> ((string->string) -> (string*string)) 
      -> (string*string) list -> (string*string) list -> string -> 
      int option -> quot

  end

structure ScsData :> SCS_DATA =
  struct

    local 
      val new_id_sql = `select scs.new_obj_id from dual`	
      fun template db_fn = (ScsError.valOf o Int.fromString) 
	(ScsError.wrapPanic db_fn new_id_sql)
    in
      fun getOracleId () = template Db.oneField
      fun getOracleIdTrans db = template (Db.Handle.oneFieldDb db)
    end


    local 
      fun gToTemplate g field_name fromString_fn = 
        ScsError.valOf' "ScsData.gToTemplate" (fromString_fn (g field_name))
    in
      fun gToInt g field_name = gToTemplate g field_name Int.fromString 
      fun gToReal g field_name = gToTemplate g field_name Real.fromString
      fun gToBool g field_name = gToTemplate g field_name Db.toBool
      fun gToDate g field_name = gToTemplate g field_name Db.toDate
      fun gToTimestamp g field_name = gToTemplate g field_name Db.toTimestamp
    end

    fun gToIntOpt    g field_name = Int.fromString (g field_name)
    fun gToRealOpt   g field_name = Real.fromString (g field_name)
    fun gToStringOpt g field_name =  ScsString.toOpt (g field_name)

    local
      fun mk_selectBoxFromDb_template sql g_fn pre_opts post_opts fv v_opt =
	let
	  val opts = 
	    pre_opts @ ( ScsError.wrapPanic (Db.list g_fn) sql ) @ post_opts
	in
	  case v_opt of
	      NONE   => ScsWidget.select opts fv
	    | SOME v => ScsWidget.selectWithDefault opts (Int.toString v) fv
	end
    in
      fun mk_selectBoxFromDb sql g_fn fv v_opt = 
        mk_selectBoxFromDb_template sql g_fn [] [] fv v_opt 
      fun mk_selectBoxFromDb' sql g_fn add_opts fv v_opt = 
        mk_selectBoxFromDb_template sql g_fn add_opts [] fv v_opt
      fun mk_selectBoxFromDb'' sql g_fn pre_opts post_opts fv v_opt = 
        mk_selectBoxFromDb_template sql g_fn pre_opts post_opts fv v_opt
    end

  end
