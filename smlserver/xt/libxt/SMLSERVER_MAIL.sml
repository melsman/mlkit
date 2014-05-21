signature SMLSERVER_MAIL = sig
  val sendmail : {to: string list, cc: string list, 
		  bcc: string list, from: string, 
		  subject: string, body: string,
		  extra_headers: string list} -> unit 
  val send     : {to: string, from: string, 
		  subject: string, body: string} -> unit
end

(*
 [sendmail {to,cc,bcc,from,subject,body,extra_headers}] sends 
 an email to the addresses in to, cc, and bcc.

 [send {to,from,subject,body}] abbreviated version of 
 sendmail.
*)
