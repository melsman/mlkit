signature WEB_INFO = sig
(*  val configFile          : unit -> string
  val configGetValue      : {sectionName: string, 
			     key: string} -> string option
  val configGetValueExact : {sectionName: string, 
			     key: string} -> string option
  val homePath            : unit -> string
  val errorLog            : unit -> string
*)
  structure Type : WEB_SERIALIZE
  val hostname            : unit -> string
  val pid                 : unit -> int
  val uptime              : unit -> int
  val configGetValue      : ('a Type.Type * string) -> 'a option
  val configSetValue      : ('a Type.Type * string * 'a) -> unit
  val configSetSpecialValue : ((('a Type.Type * string * 'a) ->
                                 unit)
                               * 'a Type.Type * string * 'a) ->
                                 unit
  val pageRoot            : unit -> string
  val getAuxConfigData    : unit -> string option
end

(*
 [configFile()] returns the location of the configuration file.

 [configGetValue{sectionName,key}] returns SOME s, if s is the
 string associated with the (sectionName, key) pair in the
 configuration file.  Returns NONE, otherwise. Case
 insensitive on sectionName, key.

 [configGetValueExact{sectionName,key}] as configGetValue, but
 case sensitive.

 [errorLog()] returns the location of the log file.
 
 [homePath()] returns the directory where the web-server is
 installed.

 [hostname()] returns the host name of the machine.

 [pid()] returns the process id of the server process.

 [uptime()] returns the number of seconds the server process
 has been running.

 [pageRoot()] returns the directory for which the server
 serves pages.

 [getAuxConfigData()] returns some string if SmlAuxData is defined 
 in you webserver configuration file and NONE otherwise.

*)
