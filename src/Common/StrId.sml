(* Exception constructors - Definition v3 page 4 *)

functor StrId(structure Timestamp: TIMESTAMP
	      structure Crash: CRASH
	     ): STRID =
  struct
    
    datatype strid = STRID of string

    fun pr_StrId(STRID str) = str

    datatype longstrid = LONGSTRID of strid list * strid

    fun pr_LongStrId(LONGSTRID(strids, strid)) =
      let
	val strings = (map (fn s => pr_StrId s ^ ".") strids)
      in
	foldr (op ^) (pr_StrId strid) strings
      end

    fun implode_longstrid (strid_list, strid) = LONGSTRID(strid_list, strid)

    fun explode_longstrid (LONGSTRID(strid_list, strid)) = (strid_list, strid)

    val mk_StrId = STRID

    fun mk_LongStrId strs =
      case (rev strs)
	of nil => Crash.impossible "StrId.mk_LongStrId"
	 | (x :: xs) => LONGSTRID(map STRID (rev xs), STRID x)

    fun inventStrId() =
      STRID("<unique_StrId." ^ Timestamp.print(Timestamp.new()) ^ ">")

    fun invented_StrId (STRID s) : bool =  (* only invented strids may start with `<' *)
      String.isPrefix "<" s   (*(String.nth 0 s = "<")*)
      (*handle _ => Crash.impossible "StrId.invented_StrId"*)

    fun longStrIdOfStrId strid = LONGSTRID(nil, strid)

    val op < = fn (STRID str1, STRID str2) => str1 < str2
  end;
