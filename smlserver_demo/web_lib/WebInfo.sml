
functor WebInfo (type conn = int
		val getReqRec : unit -> conn
		val isNull : string -> bool
    val log : string -> unit
    exception Forbidden) : WEB_INFO =
  struct
  structure Type = WebSerialize

  fun pid() : int = prim("apsml_getpid", getReqRec())

  val bbb = #name Type.Bool

  fun uptime () : int = prim("apsml_getuptime", getReqRec())

  fun pageRoot() : string =
    prim("apsml_PageRoot", (getReqRec()))

  fun configGetValue(rangeType : 'a Type.Type, k : string) = 
  let val res : string = prim("apsml_conflookup", (k, getReqRec()))
  in if isNull res  then NONE else 
       let (*val _ = log("configGetValue: " ^ res) *)
           val (a,b) = Substring.splitl (fn x => x <> #":") (Substring.all res)
           val c = Substring.triml 1 b
           val _ = case Substring.compare (a, Substring.all(#name rangeType))
             of EQUAL => ()
              | _ => raise Domain
       in 
       SOME(#from_string rangeType (Substring.string c))
       end
  end

(*
  val safeNameCheck = RegExp.fromString "[ -9;-~]+" (* ascii 0x20 - 0x39 or 0x3B - 0x7E *) 
*)                                                    (* Thus any printable char <> :     *)
	
	fun configSetValue (rangeType : 'a Type.Type, key, value : 'a) : unit = 
    let val s' : string = #to_string rangeType value
        val s : string =  #name rangeType ^":"^ s'
        fun setvalInt (k:string,v:string,e:int) = 
	         prim("apsml_confinsert", (k, v, e, getReqRec())) : unit handle Overflow => raise Forbidden 
        fun setvalString (k:string,v:string,e:string) = 
	         prim("apsml_confinsert", (k, v, e, getReqRec())) : unit handle Overflow => raise Forbidden 
        fun setBool () = 
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
          else raise Forbidden
       (* val _ = log("configSetValue: " ^ s) *)
    in 
    case key of "DBLazyConnect" => setBool ()
              | "DBUserName" => setString ()
              | "DBPassWord" => setString ()
              | "DBTNSname" => setString ()
              | "DBSessionLimit" => setInt ()
              | "DBSessionMaxDepth" => setInt ()
              | _ => setvalInt(key,s,0)
    end

    fun configSetSpecialValue (g : 'a Type.Type * string * 'a -> unit, 
                                rangeType : 'a Type.Type, key : string, value : 'a) = 
          (g (rangeType,key,value); configSetValue (rangeType, key, value))

  fun hostname () : string = prim("apsml_gethost",(getReqRec()))

  end
