signature NS_MAIL =
  sig
    val sendmail : 
      {to: string list, cc: string list, 
       bcc: string list,
       from: string, subject: string, body: string,
       extra_headers: string list} -> unit 
    val send : 
      {to: string, from: string, 
       subject: string, body: string} -> unit
  end
