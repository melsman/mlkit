signature SMLSERVER_UNSAFE = sig

  exception MissingConnection

  eqtype status
  val OK             : status
  val ERROR          : status
  val END_DATA       : status

  val write          : string -> status
  val returnFileMime : string -> string -> status
  val returnFile     : string -> status

  val formvar        : string -> string option
  val formvarAll     : string -> string list 

  val registerTrap   : string -> unit
  val scheduleScript : string -> int -> unit
  val scheduleDaily  : string -> {hour:int, minute:int} 
                       -> unit
  val scheduleWeekly : string -> {day:int, hour:int, minute:int}
                       -> unit
end

(*
 [status] abstract type of status code returned by
 functions.

 [OK] status code indicating success.

 [ERROR] status code indicating failure.

 [END_DATA] status code indicating end of data.

 [return q] sends string q to browser with status code 
 200, adding HTTP headers. Returns OK on success and ERROR 
 on failure.
 
 [write q] sends string q to browser. Returns OK on success 
 and ERROR on failure.

 [returnFileMime mimetype file] returns the entire contents 
 of the given file to the client. In addition to setting the 
 HTTP status response line to 200 and the Content-Type header 
 from the given parameter, the function also uses the stat 
 system call to generate the appropriate Last-Modified and
 Content-Length headers. The function returns a status of OK
 or ERROR.

 [returnFile file] as returnFileMime, but gets the 
 Content-Type (mimetype) argument from calling the function
 getMimeType with the given file as parameter.

 [registerTrap p] after a call to this function, requests for
 files that matches the path p, which may contain globs, are 
 trapped. The effect of a file being trapped is that the 
 script ../sys/trap.sml is executed instead. Usually, calls 
 to the registerTrap function appears in the initialization 
 script ../sys/init.sml to control access to web content.

 [scheduleScript f d] after a call to this function, the 
 script determined by the file f is scheduled to execute
 every d seconds. Usually, calls to the scheduleScript 
 function appears in the initialization script 
 ../sys/init.sml to setup scheduled execution.

 [scheduleDaily f {hour,minute}] after a call to this 
 function, the script determined by the file f is scheduled
 to execute every day at the specified time (hour and 
 minute). The hour can be an integer from 0 to 23, and the 
 minute an integer from 0 to 59.

 [scheduleWeekly f {day,hour,minute}] after a call to this 
 function, the script determined by the file f is scheduled
 to execute every week at the specified time (day, hour, and 
 minute). The day can be an integer from 0 to 6, where 0 
 represents Sunday. The hour can be an integer from 0 to 23, 
 and the minute an integer from 0 to 59.
*)