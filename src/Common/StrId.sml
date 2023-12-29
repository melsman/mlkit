(* Structure identifiers *)

structure StrId :> STRID =
  struct
    type strid = string

    fun pr_StrId str = str

    type longstrid = strid list * strid

    fun pr_LongStrId (strids, strid) =
        String.concatWith "." (strids @ [strid])

    fun implode_longstrid p = p

    fun explode_longstrid p = p

    fun mk_StrId x = x

    fun mk_LongStrId strs =
        case rev strs of
            nil => Crash.impossible "StrId.mk_LongStrId"
          | (x :: xs) => (rev xs, x)

    fun inventStrId () =
        "<unique_StrId." ^ Timestamp.print(Timestamp.new()) ^ ">"

    fun invented_StrId s : bool =  (* only invented strids may start with `<' *)
        String.isPrefix "<" s

    fun longStrIdOfStrId strid = (nil, strid)

    val op < = fn (str1:string, str2) => str1 < str2

    val pu = Pickle.string
    val pu_longstrid = Pickle.pairGen0(Pickle.listGen pu, pu)

    structure Map = StringFinMap
    structure Set = StringSet
  end
