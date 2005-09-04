signature WEB_BASICS = sig 
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
    exception Forbidden
end
