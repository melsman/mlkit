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
 [configFile()] return the location of the 
 configuration file.

 [configGetValue{sectionName,key}] returns SOME s, 
 if s is the string associated with the section 
 name, key pair in the configuration file. Returns 
 NONE, otherwise. Case insensitive on sectionName, 
 key.

 [configGetValueExact{sectionName,key}] as 
 configGetValue, but case sensitive.

 [errorLog()] returns the location of the log 
 file.
 
 [homePath()] returns the directory where 
 the web-server is installed.

 [hostname()] returns the host name of the machine.

 [pid()] returns the process id of the server 
 process.

 [serverVersion()] returns the version of the
 web-server.

 [uptime()] returns the number of seconds the 
 server process has been running.

 [pageRoot()] returns the directory for which 
 the server serves pages.
*)
