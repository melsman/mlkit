signature NS_INFO =
  sig
    val configFile          : unit -> string
    val configGetValue      : {sectionName: string, key: string} 
                              -> string option
    val configGetValueExact : {sectionName: string, key: string} 
                              -> string option
    val errorLog            : unit -> string
    val homePath            : unit -> string
    val hostname            : unit -> string
    val pid                 : unit -> int
    val serverVersion       : unit -> string
    val uptime              : unit -> int
    val pageRoot            : unit -> string
  end

(*
 [configFile()] return the location of the configuration file.

 [configGetValue{sectionName,key}] returns SOME s, if s is the 
 string associated with the section name, key pair in the 
 configuration file. Returns NONE, otherwise. Case insensitive 
 on sectionName, key.

 [configGetValueExact{sectionName,key}] as configGetValue, but 
 case sensitive.

 [errorLog()] returns the location of the log file.
 
 [homePath()] returns the directory where AOLserver is 
 installed.

 [hostname()] returns the host name of the machine.

 [pid()] returns the process id of the AOLserver process.

 [serverVersion()] returns the version of AOLserver.

 [uptime()] returns the number of seconds the AOLserver 
 process has been running.

 [pageRoot()] returns the directory for which AOLserver 
 serves pages.
*)

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

    fun serverVersion() : string =
      prim("nssml_InfoServerVersion", ())

    fun uptime() : int =
      prim("Ns_InfoUptime", ())
      
    fun pageRoot() : string =
      prim("nssml_PageRoot", ())

  end
