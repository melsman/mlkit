(* Identifiers - variables or constructors *)

structure Ident :> IDENT where type strid = StrId.strid =
  struct

    type strid = StrId.strid

    type id = string

    fun pr_id str = str

    datatype longid = LONGID of id list * id

    fun pr_longid (LONGID(ids, id)) =
      let
	val strings = (map (fn s => pr_id s ^ ".") ids)
      in
	foldr (op ^) (pr_id id) strings
      end

    fun unqualified longid =
      case longid
	of LONGID(nil, _) => true
	 | _ => false

    fun decompose (LONGID(ids, id)) =
      (map StrId.mk_StrId ids, id)

    fun decompose0 longid =
      case decompose longid
	of (nil, id) => id
	 | _ => Crash.impossible "Ident.decompose0"

    fun implode_LongId (strids,id) =
      LONGID(map StrId.pr_StrId strids,id)

    val op < : id * id -> bool = op <

   (* Identifiers needed for derived forms: *)

    val id_NIL = "nil"
    and id_CONS = "::"
    and id_TRUE = "true"
    and id_FALSE = "false"
    and id_REF = "ref"
    and id_PRIM = "prim"
    and id_EXPORT = "_export"
    and id_IT = "it"

    and id_QUOTE = "QUOTE"
    and id_ANTIQUOTE = "ANTIQUOTE"

    and id_INTINF = "_IntInf"

    (* Identifiers for predefined overloaded variables *)
    val id_ABS = "abs"
    val id_NEG = "~"
    val id_DIV = "div"
    val id_MOD = "mod"
    val id_PLUS = "+"
    val id_MINUS = "-"
    val id_MUL = "*"
    val id_LESS = "<"
    val id_GREATER = ">"
    val id_LESSEQ = "<="
    val id_GREATEREQ = ">="

    (* other identifiers in the initial basis *)

    val id_Div = "Div"
    val id_Match = "Match"
    val id_Bind = "Bind"
    val id_Overflow = "Overflow"
    val id_Interrupt = "Interrupt"
    val id_Subscript = "Subscript"
    val id_Size = "Size"

    (* misc: *)

    val bogus = LONGID (nil, "<bogus>")
    val resetRegions = "resetRegions"
    val forceResetting = "forceResetting"

    val mk_Id = fn x => x

    fun mk_LongId strs =
      case (rev strs)
	of nil => Crash.impossible "Ident.mk_LongId"
	 | x :: xs => LONGID(rev xs, x)

    local
      val initial = 0
      val count = ref initial
      fun unique() = (count := !count + 1;
		      "var" ^ Int.toString (!count))
      fun unique_named (s:string) =
                     (count := !count + 1;
		      s (*^ Int.toString (!count)*))
    in
      fun reset () = count := initial
      val inventId = unique
      fun invent_named_id (name:string) = unique_named name
      fun inventLongId() = LONGID(nil, inventId())
    end

    fun idToLongId id = LONGID(nil, id)

    val pu = Pickle.string

    structure Map = StringFinMap
    structure Set = StringSet

  end
