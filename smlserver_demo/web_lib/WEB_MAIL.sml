signature WEB_MAIL = sig
  type email = {to: string list, cc: string list, 
                bcc: string list, from: string, 
                subject: string, body: string,
                extra_headers: string list}
  val sendmail : email -> unit 
  val send     : {to: string, from: string, 
		  subject: string, body: string} -> unit
  datatype CharSet = UTF8 | ISO88591 | USASCII
  val mail : ('a -> (email * 'a * CharSet) option) ->
             ((email * (string * string) list * 'b) -> 'b) ->
             'a -> 'b -> ('a * 'b)
end

(*
 [sendmail {to,cc,bcc,from,subject,body,extra_headers}] sends 
 an email to the addresses in to, cc, and bcc.

 [send {to,from,subject,body}] abbreviated version of 
 sendmail.

 [mail f g a b] Advanced mail interface that supports sending of
 multiple emails using one SMTP connection.
*)
