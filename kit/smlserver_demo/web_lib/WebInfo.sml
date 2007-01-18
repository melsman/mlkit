
functor WebInfo (type conn = foreignptr
		val getReqRec : unit -> conn
		val getReqRecP : unit -> foreignptr
    exception MissingConnection
    val log : string -> unit
    exception Forbidden) : WEB_INFO =
  struct
  structure Type = WebSerialize

  fun isNullFp (x : foreignptr) = prim("__is_null",x) : bool
  fun isNull (x : string) = prim("__is_null",x) : bool

  fun pid() : int = (SysWord.toInt o Posix.Process.pidToWord o Posix.ProcEnv.getpid) ()

  val bbb = #name Type.Bool

  fun uptime () : int = prim("apsml_getuptime", getReqRec())

  fun pageRoot() : string =
    prim("apsml_PageRoot", (getReqRecP()))

  fun configGetValue(rangeType : 'a Type.Type, k : string) = 
  let 
    fun res () = prim("apsml_conflookup", (k : string, getReqRec())) : string
    fun getMaxHeapPoolSz () = prim("@getMaxHeapPoolSz", ()) : int
  in 
    case k
    of "MaxHeapPoolSz" =>
          if #name rangeType = #name Type.Int
          then SOME(#from_string rangeType (#to_string Type.Int (getMaxHeapPoolSz())))
          else raise Domain
     | _ => let val res = res() in
     if isNull res then NONE else 
       let (*val _ = log("configGetValue: " ^ res) *)
           val (a,b) = Substring.splitl (fn x => x <> #":") (Substring.full res)
           val c = Substring.triml 1 b
           val _ = case Substring.compare (a, Substring.full(#name rangeType))
             of EQUAL => ()
              | _ => raise Domain
       in 
       SOME(#from_string rangeType (Substring.string c))
       end
       end
  end

  fun getAuxConfigData () = 
        let
          val a = prim("sml_getAuxData", getReqRec()) : string
        in
          if isNull a then NONE else SOME a
        end

  fun getAuthType () : string option = 
         let 
           val r = prim("@apsml_get_auth_type",getReqRecP()) : foreignptr
         in
           if isNullFp r then NONE else SOME(prim ("fromCtoMLstring", r : foreignptr) : string)
         end
  
  fun getUser () : string option = 
         let 
           val r = prim("@apsml_getuser",getReqRecP()) : foreignptr
         in
           if isNullFp r then NONE else SOME(prim ("fromCtoMLstring", r : foreignptr) : string)
         end

	fun configSetValue (rangeType : 'a Type.Type, key, value : 'a) : unit = 
    let val s' : string = #to_string rangeType value
        val s : string =  #name rangeType ^":"^ s'
        fun setvalInt (k:string,v:string,e:int) = 
	         prim("apsml_confinsert", (k, v, e, getReqRec())) : unit handle Overflow => raise Forbidden 
        fun setvalString (k:string,v:string,e:string) = 
	         prim("apsml_confinsert", (k, v, e, getReqRec())) : unit handle Overflow => raise Forbidden 
        fun setMaxHeapPoolSz i = prim("@setMaxHeapPoolSz", i : int) : unit
        fun setMaxHeapPoolSz' () = 
              if #name rangeType = #name Type.Int
              then setMaxHeapPoolSz(#from_string Type.Int s')
              else raise Forbidden
(*        fun setBool () = 
          if #name rangeType = #name Type.Bool 
          then setvalInt(key, s, if #from_string Type.Bool s' then 1 else 0)
          else raise Forbidden
        fun setString () = 
          if #name rangeType = #name Type.String 
          then setvalString(key, s,s')
          else raise Forbidden
        fun setInt () =
          if #name rangeType = #name Type.Int 
          then setvalInt(key, s, #from_string Type.Int s')
          else raise Forbidden *)
       (* val _ = log("configSetValue: " ^ s) *)
    in
    case key of
                "MaxHeapPoolSz" => setMaxHeapPoolSz'()
              | _ => setvalInt(key,s,0)
    end

    fun configSetSpecialValue (g : 'a Type.Type * string * 'a -> unit, 
                                rangeType : 'a Type.Type, key : string, value : 'a) = 
          (g (rangeType,key,value); configSetValue (rangeType, key, value))

  fun hostname () : string = prim("apsml_gethost",(getReqRec()))

  end
