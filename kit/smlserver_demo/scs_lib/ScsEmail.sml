signature SCS_EMAIL =
  sig
    type email_link = {to      : string list,
		       cc      : string list,
		       bcc     : string list,
		       subject : string,
		       title   : string}

    (* [validEmail_p email] returns true if email is a valid email
        address; otherwise returns false *)
    val validEmail_p : string -> bool

    (* [genEmailLink e_link] returns a string representing an email
        link. *)
    val genEmailLink : email_link -> string
  end

structure ScsEmail :> SCS_EMAIL =
  struct
    type email_link = {to      : string list,
		       cc      : string list,
		       bcc     : string list,
		       subject : string,
		       title   : string}

    fun validEmail_p email =
      RegExp.match (RegExp.fromString "[^@\t ]+@[^@.\t ]+(\\.[^@.\n ]+)+") (ScsString.trim email)

    fun genEmailLink ({to,cc,bcc,subject,title}:email_link) =
      let
	val to = List.filter validEmail_p to
	val cc = List.filter validEmail_p cc
	val bcc = List.filter validEmail_p bcc

	val to_s = String.concatWith "," to
	val cc_s = 
	  if List.length cc > 0 then
	    "?cc=" ^ (String.concatWith "," cc)
	  else
	    ""
	val bcc_s =
	  if List.length bcc > 0 then
	    if List.length cc > 0 then
	      "&bcc=" ^ (String.concatWith "," bcc)
	    else
	      "?bcc=" ^ (String.concatWith "," bcc)
	  else
	    ""
	val subject = ScsString.trim subject
	val subject =
	  if String.size subject > 0 then
	    if List.length cc > 0 orelse List.length bcc > 0 then
	      "&subject=" ^ subject 
	    else
	      "?subject=" ^ subject
	  else
	    ""
      in
	if List.length (to @ cc @ bcc) < 250 then
	  Quot.toString
	  `<a href="mailto:^(to_s)^(cc_s)^(bcc_s)^subject">^title</a>`
	else
	  ScsDict.s [(ScsLang.da,`Der er for mange emails til at vi kan lave et "email" link.`),
		     (ScsLang.en,`Too many emails to be included in a email link.`)]
      end
    
  end