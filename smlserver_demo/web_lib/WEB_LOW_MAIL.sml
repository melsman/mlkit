signature WEB_LOW_MAIL = sig
  type MX_FQDN
  type mailer
  exception ConnectionErr of 
	    (string * (string * string) list 
	     * (string * string) list * (string * string) list)
  val getFQDN_MX : string -> (int * int * MX_FQDN) list 
  val FQDN_MX_toString : MX_FQDN -> string
  val FQDN_MX_fromString : string -> MX_FQDN
  val FQDN_MX_compare : MX_FQDN * MX_FQDN -> order
  val initConn : MX_FQDN -> mailer 
  val sendmail : string list * string * string * mailer -> 
		 (string *string) list * (string * string) list 
		 * (string * string) list
  val closeConn : mailer -> unit
  val getDefaultServer : unit -> MX_FQDN option
end

(* 
 [MX_FQDN] represents a Fully Qualified Domain Name for an MX record
 (an smtp server).  

 [mailer] represents the mail connection (socket and protocol state).  

 [ConnectionErr] may be raised by initConn and sendmail if the mailer has
 problems and needs to call closeConn.

 [getFQDN_MX] take a domain names and returns a list of triplets where
 the first element is the preference, the second element is the time
 to live and the third element is the Fully Qualified Domain Name for
 the mail server to use if such exists. The list will be prioritized
 with the best server as the first element, second best server as the
 second element, etc.

 [initConn] initializes a connection with the server represented by
 the FQDN and associates the exception with the connection. initConn
 and sendmail may raise the exception.

 [sendmail] send mails to the people through the connection given by
 the mailer.  sendmail returns the id on the mails that was accepted
 by the mail server.

 [closeConn] closes the connection given by mailer and returns three
 lists. The first list contain ids on mails accepted by the mail
 server, the second list contain ids on mails that was temporary
 undeliverable and the third list contain ids on permanent
 undeliverable mails.
*)
