signature WEB_INFO = sig
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
  val getUser             : unit -> string option
  val getAuthType         : unit -> string option
end

(*
 [hostname()] returns the host name of the machine.

 [pid()] returns the process id of the server process.

 [uptime()] returns the number of seconds the server process
 has been running.

 [configGetValue(T,key)] fetches value of type T associated with key
 if it exists.

 [configSetValue(T,key,v)] associates with key the value v of type T.

 [pageRoot()] returns the directory for which the server
 serves pages.

 [getAuxConfigData()] returns some string if SmlAuxData is defined 
 in you webserver configuration file and NONE otherwise.

 [getUser()] returns SOME username if an authentication check has
 succeeded. Returns NONE otherwise.

 [getAuthType()] returns SOME authtype if an authentication check of
 type authtype has succeeded. Returns NONE otherwise.
*)
