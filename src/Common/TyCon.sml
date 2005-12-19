(* Type constructors - Definition v3 page ?? *)

structure TyCon: TYCON =
  struct

    type strid = StrId.strid

    datatype tycon = TYCON of string

    fun pr_TyCon(TYCON str) = str

    datatype longtycon = LONGTYCON of strid list * tycon

    fun pr_LongTyCon (LONGTYCON(strid_list, tycon)) =
      let
	val string_list = (map (fn s => StrId.pr_StrId s ^ ".") strid_list)

	fun join [] = ""
	  | join (s :: rest) = s ^ join rest
      in
	join string_list ^ pr_TyCon tycon
      end


    fun implode_LongTyCon (strid_list, tycon) =
      LONGTYCON(strid_list, tycon)

    fun explode_LongTyCon (LONGTYCON(strid_list, tycon)) =
      (strid_list, tycon)

    val tycon_INT    = TYCON "int"
    val tycon_INT31  = TYCON "int31"
    val tycon_INT32  = TYCON "int32"
    val tycon_INTINF = TYCON "intinf"
    val tycon_WORD   = TYCON "word"
    val tycon_WORD8  = TYCON "word8"
    val tycon_WORD31  = TYCON "word31"
    val tycon_WORD32 = TYCON "word32"
    val tycon_REAL   = TYCON "real"
    val tycon_STRING = TYCON "string"
    val tycon_CHAR   = TYCON "char"
    val tycon_EXN    = TYCON "exn"
    val tycon_REF    = TYCON "ref"
    val tycon_BOOL   = TYCON "bool"
    val tycon_LIST   = TYCON "list"
    val tycon_FRAG   = TYCON "frag"
    val tycon_ARRAY  = TYCON "array"
    val tycon_VECTOR = TYCON "vector"
    val tycon_CHARARRAY = TYCON "chararray"
    val tycon_FOREIGNPTR = TYCON "foreignptr"
    val tycon_INSTREAM = TYCON "instream"
    val tycon_OUTSTREAM = TYCON "outstream"
    val tycon_UNIT   = TYCON "unit"

    val mk_TyCon = TYCON

    fun mk_LongTyCon ids =
      case rev ids
	of t :: strs =>
	     let
	       val strids = map StrId.mk_StrId (rev strs)
	     in
	       LONGTYCON(strids, TYCON t)
	     end

	 | nil => Crash.impossible "TyCon.mk_LongTyCon"

    val op < = fn (TYCON str1, TYCON str2) => str1 < str2

    fun is_'true'_'nil'_etc tycon =
      case tycon
	of TYCON "true" => true
	 | TYCON "false" => true
	 | TYCON "nil" => true
	 | TYCON "::" => true
	 | TYCON "ref" => true
	 | TYCON _ => false

    fun is_'it' (TYCON "it") = true
      | is_'it' (TYCON _) = false

    val pu = Pickle.convert (TYCON, fn TYCON s => s) Pickle.string
  end
