(* Identifiers - variables or constructors *)

functor Ident(structure StrId: STRID
	      structure Crash: CRASH
	     ): IDENT =
  struct

    type strid = StrId.strid

    datatype id = ID of string

    fun pr_id(ID str) = str

    datatype longid = LONGID of id list * id

    fun pr_longid(LONGID(ids, id)) =
      let
	val strings = (map (fn s => pr_id s ^ ".") ids)
      in
	foldr (op ^) (pr_id id) strings
      end

    fun unqualified longid =
      case longid
	of LONGID(nil, _) => true
	 | _ => false

    fun decompose(LONGID(ids, id)) =
      (map (fn ID x => StrId.mk_StrId x) ids, id)

    fun decompose0 longid =
      case decompose longid
	of (nil, id) => id
	 | _ => Crash.impossible "Ident.decompose0"

    fun implode_LongId(strids,id) =
      LONGID(map (ID o StrId.pr_StrId) strids,id)

    val op < = fn (ID str1, ID str2) => str1 < str2

   (* Identifiers needed for derived forms: *)

    val id_NIL = ID "nil"
    and id_CONS = ID "::"
    and id_TRUE = ID "true"
    and id_FALSE = ID "false"
    and id_REF = ID "ref"
    and id_PRIM = ID "prim"
    and id_IT = ID "it"

    and id_QUOTE = ID "QUOTE"
    and id_ANTIQUOTE = ID "ANTIQUOTE"

    (* Identifiers for predefined overloaded variables *)
    val id_ABS = ID "abs"
    val id_NEG = ID "~"
    val id_DIV = ID "div"
    val id_MOD = ID "mod"
    val id_PLUS = ID "+"
    val id_MINUS = ID "-"
    val id_MUL = ID "*"
    val id_LESS = ID "<"
    val id_GREATER = ID ">"
    val id_LESSEQ = ID "<="
    val id_GREATEREQ = ID ">="

    (* other identifiers in the initial basis *)

    val id_Div = ID "Div"
    val id_Match = ID "Match"
    val id_Bind = ID "Bind"
    val id_Overflow = ID "Overflow"
    val id_Interrupt = ID "Interrupt"

    (* misc: *)

    val bogus = LONGID (nil, ID "<bogus>")
    val resetRegions = ID "resetRegions"
    val forceResetting = ID "forceResetting"

    val mk_Id = ID

    fun mk_LongId strs =
      case (rev strs)
	of nil => Crash.impossible "Ident.mk_LongId"
	 | x :: xs => LONGID(map ID (rev xs), ID x)

    local
      val initial = 0
      val count = ref initial
      fun unique() = (count := !count + 1;
		      "var" ^ Int.toString (!count))
      fun unique_named (s:string) = 
                     (count := !count + 1;
		      s (*^ Int.toString (!count)*))
    in
      fun reset() = count := initial
      val inventId = ID o unique
      fun invent_named_id (name:string) = ID(unique_named name)
      fun inventLongId() = LONGID(nil, inventId())
    end

    fun idToLongId id = LONGID(nil, id)

    val pu = Pickle.convert (ID, fn ID s => s) Pickle.string

  end;
