
structure NsInfo : NS_INFO =
  struct
    fun isNull(s : string) : bool = prim("nssml_isNullString", s)
    fun configFile() : string =
      prim("nssml_InfoConfigFile", ())

    fun configGetValue {sectionName: string, key: string} : string option =
      let val res : string = prim("nssml_configGetValue", 
				  (sectionName, key))
      in if isNull res then NONE
	 else SOME res
      end

    fun configGetValueExact {sectionName: string, key: string} : string option =
      let val res : string = prim("nssml_configGetValueExact", 
				  (sectionName, key))	  
      in if isNull res then NONE
	 else SOME res
      end
	    
    fun errorLog() : string =
      prim("nssml_InfoErrorLog", ())

    fun homePath() : string =
      prim("nssml_InfoHomePath", ())

    fun hostname() : string =
      prim("nssml_InfoHostname", ())

    fun pid() : int =
      prim("Ns_InfoPid", ())

    fun uptime() : int =
      prim("Ns_InfoUptime", ())
      
    fun pageRoot() : string =
      prim("nssml_PageRoot", ())

  end
