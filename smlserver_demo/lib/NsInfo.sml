signature NS_INFO =
  sig
    val configFile          : unit -> string
    val configGetValue      : {sectionName: string, key: string} -> string option
    val configGetValueExact : {sectionName: string, key: string} -> string option
    val errorLog            : unit -> string
    val homePath            : unit -> string
    val hostname            : unit -> string
    val pid                 : unit -> int
    val serverVersion       : unit -> string
    val uptime              : unit -> int
    val pageRoot            : unit -> string
  end

structure NsInfo : NS_INFO =
  struct
    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)
    fun configFile() : string =
      prim("nssml_InfoConfigFile", "nssml_InfoConfigFile", ())

    fun configGetValue {sectionName: string, key: string} : string option =
      let val res : string = prim("nssml_configGetValue", "nssml_configGetValue", 
				  (sectionName, key))
      in if isNull res then NONE
	 else SOME res
      end

    fun configGetValueExact {sectionName: string, key: string} : string option =
      let val res : string = prim("nssml_configGetValueExact", "nssml_configGetValueExact", 
				  (sectionName, key))	  
      in if isNull res then NONE
	 else SOME res
      end
	    
    fun errorLog() : string =
      prim("nssml_InfoErrorLog", "nssml_InfoErrorLog", ())

    fun homePath() : string =
      prim("nssml_InfoHomePath", "nssml_InfoHomePath", ())

    fun hostname() : string =
      prim("nssml_InfoHostname", "nssml_InfoHostname", ())

    fun pid() : int =
      prim("Ns_InfoPid", "Ns_InfoPid", ())

    fun serverVersion() : string =
      prim("nssml_InfoServerVersion", "nssml_InfoServerVersion", ())

    fun uptime() : int =
      prim("Ns_InfoUptime", "Ns_InfoUptime", ())
      
    fun pageRoot() : string =
      prim("nssml_PageRoot", "nssml_PageRoot", ())

  end
