signature FORMVAR =
  sig
    (* Checking form variables are very important but also 
       tedious, because the same kind of code is written 
       in almost every file. This module overcomes the 
       problem by defining several functions which can be used
       to test the form variables used throughout the system.

       The idea is that you define a list of form types, 
       corresponding to the values used in forms. For instance, 
       you may have an email type representing all the 
       email-values. For every form type, you define a
       function getFormtype, that takes three arguments, 
         (1) the name of the form-variable holding the value, 
         (2) the name of the field in the form; the user may be
             presented an errorpage with more than one error
             and it is important that the error message 
	     refer to a given field in the form
         (3) an error container of type errs used to hold 
	     the error messages sent back to the user.
       The type formvar_fn represents the type of 
       functions used to check form variables. The functions
       are named getFormtypeErr.

       When you have checked all the form values, you can 
       call the function any_errors, which will return an 
       error-page if any errors happended and otherwise 
       do nothing. If an error-page is returned, then
       the process.is terminated.

       If you do not want an error page returned to the user
       then use one of the wrapper functions:

         wrapOpt : on success returns SOME v where v is the 
                   form value; otherwise NONE
         wrapExn : raises exception FormVar if it fails to
                   parse the form variable
         wrapPanic: executes a function on fail; this may
                    be used to control system failures. Say,
                    you have a hidden form variable seq_id
                    (a sequence id in the database) and it
                    can't be parsed then the function may
                    log the error, send mail to the system
                    maintainer etc. 
         wrapFail: on failure, a page is returned. The 
                   difference from the getFormtypeErr 
                   functions is that with wrapFail only
                   one error is shown on the error page
                   at the time.
       The file /www/formvar_chk shows how to use the 
       wrap-functions. *)
	
    exception FormVar of string
    type quot = string frag list
    type errs = quot list
    type 'a formvar_fn = string * string * errs -> 'a * errs

    val emptyErr : errs
    val addErr : quot * errs -> errs
    val anyErrors : errs -> unit

    val getIntErr : int formvar_fn
    val getNatErr : int formvar_fn
    val getRealErr : real formvar_fn
    val getStringErr : string formvar_fn
    val getIntRangeErr : int -> int -> int formvar_fn
    val getEmailErr : string formvar_fn 
    val getNameErr : string formvar_fn 
    val getAddrErr : string formvar_fn
    val getLoginErr : string formvar_fn
    val getPhoneErr : string formvar_fn
    val getHtmlErr : string formvar_fn
    val getUrlErr : string formvar_fn
    val getCprErr : string formvar_fn
    val getEnumErr : string list -> string formvar_fn
    val getDateIso : string formvar_fn

    val wrapOpt : 'a formvar_fn -> (string -> 'a option)
    val wrapExn : 'a formvar_fn -> (string -> 'a)
    val wrapFail : 'a formvar_fn -> (string * string -> 'a)
    val wrapPanic : (string * string -> 'a) -> 'a formvar_fn -> (string -> 'a)
  end

structure FormVar :> FORMVAR =
  struct
    type quot = string frag list
    type errs = quot list
    type 'a formvar_fn = string * string * errs -> 'a * errs
    exception FormVar of string

    val emptyErr : errs = []

    fun addErr (emsg:quot,errs:errs) = emsg :: errs
    fun genErrMsg (f_msg:string,msg:quot) : quot = `Error in field ^f_msg. ` ^^ msg
    fun errNoFormVar(f_msg:string,ty:string) : quot = `Error in field ^f_msg. You must provide a <b>^ty</b>.`
    fun errTypeMismatch(f_msg:string,ty:string,v:string) : quot = 
      `Error in field ^f_msg. You must provide a <b>^ty</b> - <i>^v</i> is not a ^ty.`
    fun errTooLarge(f_msg:string,ty:string,v:string) : quot =
      `Error in field ^f_msg. The provided ^ty (<i>^v</i>) is too large.`
    fun errTooMany(f_msg:string) : quot =
      `Error in field ^f_msg. More than one dataitem is provided.`

    fun genErrorPg (errs: errs): quot =
      `<html>
      <head><title>Form Error</title></head>
      <body bgcolor=white>
      We had a problem processing your entry:

      <ul>` ^^
      Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^
      `</ul>

      Please back up using your browser, correct it, and resubmit your entry<p>

      Thank you.
      <hr> <i>Served by SMLserver</i>
      </body>
      </html>`

    fun anyErrors ([]:errs) = ()
      | anyErrors (errs) = (Ns.return (genErrorPg errs); Ns.exit())

    fun wrapOpt (f : 'a formvar_fn) : string -> 'a option =
      fn fv => 
      case f (fv,"",[]) of
	(v,[]) => SOME v
      | _ => NONE

    fun wrapExn (f : 'a formvar_fn) : string -> 'a =
      fn fv =>
      case f (fv,fv,[]) of
	(v,[]) => v
      | (_,x::xs) => raise FormVar (Quot.toString x)

    fun wrapFail (f : 'a formvar_fn) : string * string -> 'a =
      fn (fv:string,emsg:string) =>
      case f (fv,emsg,[]) of
	(v,[]) => v
       | (_,errs) => (Ns.return(genErrorPg errs); Ns.exit())

    fun wrapPanic (f_panic: string * string -> 'a) (f : 'a formvar_fn) : string -> 'a =
      fn fv =>
      case f (fv,fv,[]) of
	(v,[]) => v
       | (_,x::xs) => f_panic(fv,Quot.toString x)

    local
      fun getErrWithOverflow (empty_val:'a) (ty:string) (chk_fn:string->'a option) =
	fn (fv:string,emsg:string,errs:errs) =>
	(case Ns.Conn.formvarAll fv of
	   [] => (empty_val,addErr(errNoFormVar(emsg,ty),errs))
	 | [""] => (empty_val,addErr(errNoFormVar(emsg,ty),errs))
	 | [v] =>
	     (case chk_fn v of
		SOME v => (v,errs)
	      | NONE => (empty_val, addErr(errTypeMismatch(emsg,ty,v),errs)))
		handle Overflow => (empty_val, addErr(errTooLarge(emsg,ty,v),errs))
	 | _ => (empty_val, addErr(errTooMany emsg,errs)))
    in
      val getIntErr = getErrWithOverflow 0 "number"
	(fn v => let val l = explode v
		 in 
		   case l
		     of c::_ => 
		       if Char.isDigit c orelse c = #"-" orelse c = #"~" then
			 (case Int.scan StringCvt.DEC List.getItem l
			    of SOME (n, nil) => SOME n
			  | _ => NONE)
		       else NONE
		   | nil => NONE
		 end)
      val getNatErr = getErrWithOverflow 0 "positive number" 
	(fn v => let val l = explode v
		 in 
		   case l
		     of c::_ => 
		       if Char.isDigit c then
			 (case Int.scan StringCvt.DEC List.getItem l
			    of SOME (n, nil) => SOME n
			  | _ => NONE)
		       else NONE
		   | nil => NONE
		 end)
	  
      val getRealErr = getErrWithOverflow 0.0 "real"
	(fn v => let val l = explode v
		 in
		   case l
		     of c::_ => 
		       if Char.isDigit c orelse c = #"-" orelse c = #"~" then
			 (case Real.scan List.getItem l
			    of SOME (n, nil) => SOME n
			  | _ => NONE)
		       else NONE
		   | nil => NONE
		 end)

      val getStringErr = getErrWithOverflow "" "string" (fn v => if size v = 0 then NONE else SOME v)
    end

    fun getIntRangeErr a b (args as (fv:string,emsg:string,errs:errs)) =
      let
	val (i,errs') = getIntErr args
      in
	if List.length errs = List.length errs' then
	  if a <= i andalso i <= b 
	    then (i,errs)
	  else (0,addErr(genErrMsg(emsg,`The integer <i>^(Int.toString i)</i> is not within the valid
				   range [^(Int.toString a),...,^(Int.toString b)].`),errs))
	else
	  (0,errs')
      end
    
    local
      fun trim s = Substring.string (Substring.dropr Char.isSpace (Substring.dropl Char.isSpace (Substring.all s)))
      fun getErr (empty_val:'a) (conv_val:string->'a) (ty:string) (add_fn:string->quot) (chk_fn:string->bool) =
	fn (fv:string,emsg:string,errs:errs) =>
	case Ns.Conn.formvarAll fv of
	  []  => (empty_val,addErr(genErrMsg(emsg,add_fn ("You must provide an <b>"^ty^"</b>.")),errs))
	| [""]  => (empty_val,addErr(genErrMsg(emsg,add_fn ("You must provide an <b>"^ty^"</b>.")),errs))
	| [v] => 
	    if chk_fn v then
	      (conv_val v,errs)
	    else
	      (empty_val, addErr(genErrMsg(emsg,add_fn ("You must provide an valid <b>"^ty^"</b> - <i>" ^ 
							v ^ "</i> is not one")),
				 errs))
	| _ => (empty_val, addErr(errTooMany emsg,errs))
      val getErr' = getErr "" trim
      fun msgEmail s = `^s
	<blockquote>A few examples of valid emails:
	<ul>
	<li>login@it-c.dk
	<li>user@supernet.com
	<li>FirstLastname@very.big.company.com\n
	</ul></blockquote>`
      fun msgName s = `^s
	<blockquote>
	A name may contain the letters from the alphabet including: <b>'</b>, <b>\</b>,<b>-</b>,<b>æ</b>,
	<b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b> and space.
	</blockquote>`
      fun msgAddr s = `^s
	<blockquote>
	An address may contain digits, letters from the alphabet including:
	<b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>,
	<b>æ</b>,<b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>
	</blockquote>`
      fun msgLogin s = `^s
	<blockquote>
	A login may contain lowercase letters from the alphabet and digits - the first
	character must not be a digit. Special characters 
	like <b>æ</b>,<b>ø</b>,<b>å</b>,<b>;</b>,<b>^^</b>,<b>%</b> are not alowed. 
	A login must be no more than 10 characters and at least three characters.
	</blockquote>`
      fun msgPhone s = `^s
	<blockquote>
	A telephone numer may contain numbers and letters from the alphabet 
	including <b>-</b>, <b>,</b> and <b>.</b>.
	</blockquote>`
      fun msgHTML s = `^s
	<blockquote>
	You may use the following HTML tags in your text: Not implemented yet.
	</blokcquote>`
      fun msgURL s = `^s
	<blockquote>
	<a href="/url_desc.sml">URL (Uniform Resource Locator)</a> - 
	we only support the <code>http://</code> type (e.g., <code>http://www.it.edu</code>).
	</blockquote>`
      fun msgCpr s = `^s
	<blockquote>
	If you hold a Danish CPR-number, then the format is:
	<code>DDMMYYYY-TTTT</code>, where <code>TTTT</code> are four numbers, for instance
	<code>291270-1234</code>. <p>
	  
	We also perform a <a
	href="http://www.cpr.dk/modulus11_beregn.htm">modulus 11
	check (text is in Danish)</a>.<p>

	If you do not hold a Danish CPR-nummer, then write day,
	month and year of you birthday in the order given above
	(e.g., <code>DDMMYY</code>). Thereafter write the two first
	letters in your (first) firstname and the first letter in
	your (last) surname. In the last field write 2 if you are a
	female and 1 if your are a male. <p>

	A male, with no Danish CPR-number named Claes Anders Fredrik
	Moren, born August 31, 1975 writes: <b>310875-CLM1</b>.

	</blockquote>`
          (*<blockquote>
	  Hvis du har et dansk CPR-nummer, så er formatet:
	  DDMMYYYY-TTTT, hvor TTTT er fire tal, eksempelvis
	  291270-1234. <p>Derudover udføres <a href=\"http://www.cpr.dk/modulus11_beregn.htm\">modulus 11 check</a>.
	  <p>
	  Hvis du ikke har et dansk CPR-nummer, skrives dato,
	  måned og år for din fødselsdag i den angivne
	  rækkefølge i de seks felter før stregen. I de
	  første 3 felter efter stregen skrives de to første
	  bogstaver i dit (første) fornavn efterfulgt af det
	  første bogstav i dit (sidste) efternavn. I den
	  sidste rubrik angives dit køn med 1 for mand og 2
	  for kvinde.  <p> En mand, uden dansk CPR-nummer,
	  ved navn Claes Anders Fredrik Moren, født den
	  31. august 1975, skal skrive: <b>310875-CLM1</b>.
	  </blockquote>*)
      fun msgEnum enums s = `^s
	You must choose among the following enumerations:
	<blockquote>
	^(String.concatWith "," enums)
	</blockquote>`
      fun msgDateIso s = `^s
	<blockquote>
	You must type a <b>date</b> in the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-10-25).
	</blockquote>`

      fun chkCpr cpr =
	let
	  fun mk_yyyymmdd (d1,d2,m1,m2,y1,y2) =
	    let
	      val yy = Option.valOf(Int.fromString(String.implode [y1,y2]))
	      val mm = Option.valOf(Int.fromString(String.implode [m1,m2]))
	      val dd = Option.valOf(Int.fromString(String.implode [d1,d2]))
	    in
	      if yy < 10 then
		(2000+yy,mm,dd)
	      else
		(1900+yy,mm,dd)
	    end
	  fun chk_modulus11 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) =
	    let
	      val sum1 = c1*4 + c2*3 + c3*2 + c4*7 + c5*6 + c6*5 + c7*4 + c8*3 + c9*2 + c10*1
	    in
	      Int.mod(sum1,11) = 0
	    end
	  fun cpr_ok (d1,d2,m1,m2,y1,y2,t1,t2,t3,t4) =
	    let
	      fun c2d ch = Option.valOf(Int.fromString(String.implode [ch]))
	      val (yyyy, mm, dd) = mk_yyyymmdd (d1,d2,m1,m2,y1,y2) 
	      val cpr = String.implode [d1,d2,m1,m2,y1,y2,#"-",t1,t2,t3,t4]
	    in
	      if Char.isDigit t1 andalso Char.isDigit t2 andalso Char.isDigit t3 andalso Char.isDigit t4 then
		let
		  (* DK CPR no *)
		  val (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) =
		    (c2d d1,c2d d2,c2d m1,c2d m2,c2d y1,c2d y2,c2d t1,c2d t2,c2d t3,c2d t4)
		  val tttt = Option.valOf(Int.fromString(String.implode [t1,t2,t3,t4]))
		in
		  if SmlsDate.dateOk (dd,mm,yyyy) andalso chk_modulus11(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) then 
		    true
		  else
		    false
		end
	      else
		(* NON DK CPR no *)
		if Char.isAlpha t1 andalso Char.isAlpha t2 andalso Char.isAlpha t3 andalso
		  (t4 = (#"1") orelse t4 = (#"2")) andalso
		  SmlsDate.dateOk (dd,mm,yyyy) then
		  true
		else
		  false
	    end
	in
	  case String.explode (trim cpr) of
	    d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: (#"-") :: t1 :: t2 :: t3 :: t4 :: [] =>
	      cpr_ok(d1,d2,m1,m2,y1,y2,t1,t2,t3,t4)
	  | d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: t1 :: t2 :: t3 :: t4 :: [] =>
	      cpr_ok(d1,d2,m1,m2,y1,y2,t1,t2,t3,t4)
	  | _ => false
	end
      handle _ => false
      fun chkEnum enums v =
	case List.find (fn enum => v = enum) enums
	  of NONE => false
	| SOME _ => true
      fun chkDateIso v =
	let
	  fun dateOk (d,m,y) = SmlsDate.dateOk(Option.valOf (Int.fromString d),
					       Option.valOf (Int.fromString m),Option.valOf (Int.fromString y))
	in
	  (case RegExp.regExp "^([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)$" v of
	     SOME [_,yyyy,mm,dd] => dateOk(dd,mm,yyyy)
	     | NONE => (case RegExp.regExp "^([0-9][0-9][0-9][0-9])([0-9][0-9]?)([0-9][0-9]?)$" v of
			  SOME [_,yyyy,mm,dd] => dateOk(dd,mm,yyyy)
			| _ => false))
	end
      handle _ => false
    in
      val getEmailErr = getErr' "email" msgEmail 
	(fn email =>
	 let
	   val bad_letters = "[^ @!\"#¤%&/()=?´`|¨~'*;:,æøåÅØÆ§½]"
	 in
	   RegExp.regExpBool ("^" ^ bad_letters ^ "+@" ^ bad_letters ^ "+\\." ^ bad_letters ^"+$") email
	 end)
      val getNameErr = getErr' "name" msgName (RegExp.regExpBool "^[a-zA-ZAÆØÅaæøå '\\-]+$")
      val getAddrErr = getErr' "address" msgAddr (RegExp.regExpBool "^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]+$")
      val getLoginErr = getErr' "login" msgLogin 
	(fn login =>
	 RegExp.regExpBool "^[a-z]([a-z0-9\\-]+)$" login andalso 
	 String.size login >= 3 andalso String.size login <= 10)
      val getPhoneErr = getErr' "phone number" msgPhone (RegExp.regExpBool "^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]+$")
      (* getHtml : not implemented yet *)
      val getHtmlErr = getErr' "HTML text" msgHTML (fn html => html <> "")
      val getUrlErr =  getErr' "URL" msgURL (RegExp.regExpBool "^http:[0-9]*//([-0-9a-zA-Z/\\\\\\._]+)$")
      val getCprErr = getErr' "cpr number" msgCpr chkCpr
      val getEnumErr = fn enums => getErr' "enumeration" (msgEnum enums) (chkEnum enums)
      val getDateIso = getErr' "date" msgDateIso chkDateIso
    end
  end
