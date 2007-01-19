signature WEB_LOG = sig 
  type LogSeverity 
  val Emergency : LogSeverity 
  and Alert     : LogSeverity 
  and Critical  : LogSeverity  
  and Error     : LogSeverity
  and Warning   : LogSeverity 
  and Notice    : LogSeverity 
  and Info      : LogSeverity 
  and Debug     : LogSeverity
  val log : LogSeverity * string -> unit
  val advLog : LogSeverity * 'a * ('a -> string) -> 'a
end

(*
 [LogSeverity] Type of log severity level.

 [Emergency] something extremely bad occurred.

 [Alert]

 [Critical]

 [Error] something bad occurred.

 [Warning] default logging level.

 [Notice]

 [Info]

 [Debug] lowest logging level.

 [log (ls,s)] write the string s to the log file with log
 severity ls.

 [advLog(ls,s,f)] log f(s) with log severity ls and return s.
*)
