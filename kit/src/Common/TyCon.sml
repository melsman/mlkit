(* Type constructors - Definition v3 page ?? *)

functor TyCon(structure StrId: STRID
	      structure Crash: CRASH
	     ): TYCON =
  struct

    open Edlib

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
    val tycon_WORD   = TYCON "word"
    val tycon_WORD8  = TYCON "word8"
    and tycon_REAL   = TYCON "real"
    and tycon_STRING = TYCON "string"
    and tycon_CHAR   = TYCON "char"
    and tycon_EXN    = TYCON "exn"
    and tycon_REF    = TYCON "ref"
    and tycon_BOOL   = TYCON "bool"
    and tycon_LIST   = TYCON "list"
    and tycon_WORD_TABLE = TYCON "word_table"
    and tycon_INSTREAM = TYCON "instream"
    and tycon_OUTSTREAM = TYCON "outstream"
    and tycon_UNIT   = TYCON "unit"

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

    val strings_'true'_'nil'_etc = ["true", "false", "nil", "::", "ref"]
    fun is_'true'_'nil'_etc tycon =
          List.member (pr_TyCon tycon) strings_'true'_'nil'_etc
    fun is_'it' (TYCON "it") = true
      | is_'it' (TYCON s) = false
  end;
