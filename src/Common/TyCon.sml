(* Type constructors - Definition v3 page ?? *)

structure TyCon :> TYCON where type strid = StrId.strid =
  struct

    type strid = StrId.strid

    type tycon = string

    fun pr_TyCon x = x

    type longtycon = strid list * tycon

    fun pr_LongTyCon (strid_list, tycon) =
      String.concatWith "." (map StrId.pr_StrId strid_list @ [tycon])

    fun implode_LongTyCon p = p
    fun explode_LongTyCon p = p

    val tycon_INT    = "int"
    val tycon_INT31  = "int31"
    val tycon_INT32  = "int32"
    val tycon_INT63  = "int63"
    val tycon_INT64  = "int64"
    val tycon_INTINF = "intinf"
    val tycon_WORD   = "word"
    val tycon_WORD8  = "word8"
    val tycon_WORD31 = "word31"
    val tycon_WORD32 = "word32"
    val tycon_WORD63 = "word63"
    val tycon_WORD64 = "word64"
    val tycon_REAL   = "real"
    val tycon_F64    = "f64"
    val tycon_STRING = "string"
    val tycon_CHAR   = "char"
    val tycon_EXN    = "exn"
    val tycon_REF    = "ref"
    val tycon_BOOL   = "bool"
    val tycon_LIST   = "list"
    val tycon_FRAG   = "frag"
    val tycon_ARRAY  = "array"
    val tycon_VECTOR = "vector"
    val tycon_CHARARRAY = "chararray"
    val tycon_FOREIGNPTR = "foreignptr"
    val tycon_INSTREAM = "instream"
    val tycon_OUTSTREAM = "outstream"
    val tycon_UNIT   = "unit"

    fun mk_TyCon x = x

    fun mk_LongTyCon ids =
        case rev ids of
            t :: strs =>
            let val strids = map StrId.mk_StrId (rev strs)
            in (strids, t)
            end
          | nil => Crash.impossible "TyCon.mk_LongTyCon"

    val op < = fn (str1:string, str2) => str1 < str2

    fun is_'true'_'nil'_etc tycon =
        case tycon of
            "true" => true
          | "false" => true
          | "nil" => true
          | "::" => true
          | "ref" => true
          | _ => false

    fun is_'it' "it" = true
      | is_'it' _ = false

    val pu = Pickle.string

    structure Map = StringFinMap
  end
