signature FORM_VAR =
  sig

    (* Checking form variables is an important part of implementing a
       secure and stable web-site, but it is often a tedious job,
       because the same kind of code is written in all files that
       verify form variables. This module overcomes the tedious part
       by defining several functions that may be used to test form
       variables consistently throughout a large system.

       The idea is to define a set of functions, corresponding to each
       type of value used in forms. Each function is defined to access
       values contained in form variables of the particular type. For
       instance, a function is defined for accessing all possible
       email addresses in a form variable. In case the given form
       variable does not contain a valid email address, errors are
       accumulated and presented to the user when all form variables
       have been checked. So as to deal with error accumulation
       properly, each function takes three arguments:

         (1) The name of the form-variable holding the value, 
         (2) The name of the field in the form; the user may 
	     be presented with an errorpage with more than one 
	     error and it is important that the error message 
	     refer to a given field in the form
         (3) An error container of type errs used to hold 
	     the error messages sent back to the user

       The functions are named getFormtypeErr, where Formtype range
       over possible form types. In each script, when all form
       variables have been checked using calls to particular
       getFormtypeErr functions, a call to the function any_errors
       returns an error-page if any errors occurred and otherwise
       proceeds with the remainder of the script. If an error-page is
       returned, then the script is terminated.

       The type formvar_fn represents the type of functions used to
       check form variables. If it is not desirable to return an error
       page, the programmer may use one of the wrapper functions to
       obtain appropriate behavior:

         wrapOpt : On success returns SOME v where v is the 
                   form value; otherwise NONE
         wrapExn : Raises exception FormVar if it fails to
                   parse the form variable
         wrapPanic: Executes a function on fail; this may
                    be used to control system failures. Say,
                    you have a hidden form variable seq_id
                    (a sequence id in the database) and it
                    can't be parsed then the function may
                    log the error, send mail to the system
                    maintainer etc. 
         wrapFail: On failure, a page is returned. The 
                   difference from the getFormtypeErr 
                   functions is that with wrapFail only
                   one error is shown on the error page
                   at the time.
       The file /www/formvar_chk shows how to use the 
       wrap-functions. *)
	
    exception FormVar of string
    type errs
    type 'a formvar_fn = string * string * errs -> 'a * errs

    val emptyErr       : errs
    val addErr         : Quot.quot * errs -> errs
    val buildErrMsg    : errs -> Quot.quot
    val anyErrors      : errs -> unit
    val isErrors       : errs -> bool

    val getIntErr      : int formvar_fn
    val getNatErr      : int formvar_fn
    val getRealErr     : real formvar_fn
    val getStringErr   : string formvar_fn
    val getIntRangeErr : int -> int -> int formvar_fn
    val getEmailErr    : string formvar_fn 
    val getNameErr     : string formvar_fn 
    val getAddrErr     : string formvar_fn
    val getLoginErr    : string formvar_fn
    val getPhoneErr    : string formvar_fn
    val getHtmlErr     : string formvar_fn
    val getUrlErr      : string formvar_fn
    val getEnumErr     : string list -> string formvar_fn
    val getYesNoErr    : string formvar_fn
    val getTableName   : string formvar_fn

  (*  val wrapQQ  : string formvar_fn -> (string * string) formvar_fn *)
    val wrapOpt : 'a formvar_fn -> (string -> 'a option)
    val wrapMaybe : 'a formvar_fn -> 'a formvar_fn
    val wrapExn : 'a formvar_fn -> (string -> 'a)
    val wrapFail : 'a formvar_fn -> (string * string -> 'a)
    val wrapPanic : (Quot.quot -> 'a) -> 'a formvar_fn -> (string -> 'a)
    val wrapIntAsString : int formvar_fn -> string formvar_fn

    val getStrings : string -> string list

    (* For extensions *)
    val trim : string -> string
    val getErr : 'a -> (string->'a) -> string -> (string->Quot.quot) -> (string->bool) -> 'a formvar_fn
  end

structure FormVar :> FORM_VAR =
  struct
    type quot = Quot.quot
    type errs = quot list
    type 'a formvar_fn = string * string * errs -> 'a * errs

    val regExpMatch   = RegExp.match   o RegExp.fromString
    val regExpExtract = RegExp.extract o RegExp.fromString

    exception FormVar of string

    val emptyErr : errs = []

    fun addErr (emsg:quot,errs:errs) = emsg :: errs
    fun B a = "<b>" ^ a ^ "</b>"
    fun genErrMsg (f_msg:string,msg:quot) : quot = `Error in field ^(B f_msg). ` ^^ msg
    fun errNoFormVar(f_msg:string,ty:string) : quot = `Error in field ^(B f_msg). You must provide a valid ^ty.`
    fun errTypeMismatch(f_msg:string,ty:string,v:string) : quot = 
      `Error in field ^(B f_msg). You must provide a valid ^ty - <i>^v</i> is not a valid ^ty.`
    fun errTooLarge(f_msg:string,ty:string,v:string) : quot =
      `Error in field ^(B f_msg). The provided ^ty (<i>^v</i>) is too large.`
    fun errTooMany(f_msg:string) : quot =
      `Error in field ^(B f_msg). More than one data item is provided.`

    fun buildErrMsg (errs: errs) : quot =
          `We had a problem processing your entry:
	   <ul>` ^^ 
	   Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^ `
	   </ul>
	   Please back up using your browser, correct the form, and resubmit your entry.<p>	   
	   Thank you.`

    fun returnErrors (errs: errs) = 
      (Page.return "Form Error" (buildErrMsg errs);
       Web.exit())

    fun anyErrors ([]:errs) = ()
      | anyErrors (errs) = returnErrors errs

    fun isErrors ([]:errs) = false
      | isErrors (errs) = true

    fun wrapOpt (f : 'a formvar_fn) : string -> 'a option =
      fn fv => 
      case f (fv,"",[]) of
	(v,[]) => SOME v
      | _ => NONE

    fun wrapIntAsString (f : int formvar_fn) =
      (fn (fv,emsg,errs) => 
       case f(fv,emsg,[]) of
	 (i,[]) => (Int.toString i,errs)
	|(_,[e]) => ("",addErr(e,errs))
	| _ => Page.panic `FormVar.wrapIntAsString failed on ^fv`)

    fun trim s = Substring.string (Substring.dropr Char.isSpace (Substring.dropl Char.isSpace (Substring.full s)))
    fun wrapMaybe (f : 'a formvar_fn) =
      (fn (fv,emsg,errs) => 
       (case Web.Conn.formvarAll fv of
	  [] => (case f(fv,emsg,[]) of (v,_) => (v,errs)) (* No formvar => don't report error *)
	| [v] => 
	   (if trim v = "" then
	      (case f(fv,emsg,[]) of (v,_) => (v,errs)) (* Don't report error *)
	    else f(fv,emsg,errs))
	| _ => f(fv,emsg,errs))) (* Multiple formvars => report error *)

    fun wrapExn (f : 'a formvar_fn) : string -> 'a =
      fn fv =>
      case f (fv,fv,[]) of
	(v,[]) => v
      | (_,x::xs) => raise FormVar (Quot.toString x)

    fun wrapFail (f : 'a formvar_fn) : string * string -> 'a =
      fn (fv:string,emsg:string) =>
      case f (fv,emsg,[]) of
	(v,[]) => v
       | (_,errs) => returnErrors errs

    fun wrapPanic (f_panic: quot -> 'a) (f : 'a formvar_fn) : string -> 'a =
      fn fv =>
      ((case f (fv,fv,[]) of
	(v,[]) => v
       | (_,x::xs) => f_panic(`^("\n") ^fv : ` ^^ x))
	  handle X => f_panic(`^("\n") ^fv : ^(General.exnMessage X)`))

    local

      fun getFormVar fv = Web.Conn.formvarAll fv

      fun getErrWithOverflow (empty_val:'a) (ty:string) (chk_fn:string->'a option) =
	fn (fv:string,emsg:string,errs:errs) =>
	(case getFormVar (*Web.Conn.formvarAll*) fv of
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
		 end handle Fail s => NONE)
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
	  else (0,addErr(genErrMsg(emsg,`The integer <i>^(Int.toString i)</i> is not within the valid range
				   [^(Int.toString a),...,^(Int.toString b)].`),errs))
	else
	  (0,errs')
      end
    
    fun getErr (empty_val:'a) (conv_val:string->'a) (ty:string) (add_fn:string->quot) (chk_fn:string->bool) =
      fn (fv:string,emsg:string,errs:errs) =>
      case Web.Conn.formvarAll fv of
	[]  => (empty_val,addErr(genErrMsg(emsg,add_fn ("You must provide a valid "^ty^".")),errs))
      | [""]  => (empty_val,addErr(genErrMsg(emsg,add_fn ("You must provide a valid "^ty^".")),errs))
      | [v] => 
	  if chk_fn v then
	    (conv_val v,errs)
	  else
	    (empty_val, addErr(genErrMsg(emsg,add_fn ("You must provide a valid "^ty^" - <i>" ^ 
						      v ^ "</i> is not one")),
			       errs))
      | _ => (empty_val, addErr(errTooMany emsg,errs))

    local
      val getErr' = getErr "" trim
      fun msgEmail s = 
             `^s
	     <blockquote>A few examples of valid emails:
	     <ul>
	     <li>login@it-c.dk
	     <li>user@supernet.com
	     <li>FirstLastname@very.big.company.com
	     </ul></blockquote>`

      fun msgName s = 
            `^s
	     <blockquote>
	     A name may contain the letters from the alphabet including: <b>'</b>, <b>\</b>,<b>-</b>,<b>æ</b>,
	     <b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b> and space.
	     </blockquote>`

      fun msgAddr s = 
            `^s
	     <blockquote>
	     An address may contain digits, letters from the alphabet including:
	     <b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b>, <b>;</b>, <b>,</b>,
	     <b>æ</b>,<b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>
	     </blockquote>`

      fun msgLogin s = 
            `^s
	     <blockquote>
	     A login may contain lowercase letters from the alphabet and digits - the first
	     character must not be a digit. Special characters 
	     like <b>æ</b>,<b>ø</b>,<b>å</b>,<b>;</b>,<b>^^</b>,<b>%</b> are not alowed. 
	     A login must be no more than 10 characters and at least three characters.
	     </blockquote>`

      fun msgPhone s = 
            `^s
	     <blockquote>
	     A telephone numer may contain numbers and letters from the alphabet 
	     including <b>-</b>, <b>,</b> and <b>.</b>.
	     </blockquote>`

      fun msgHTML s = 	
            `^s
	     <blockquote>
	     You may use the following HTML tags in your text: Not implemented yet.
	     </blokcquote>`

      fun msgURL s = 
            `^s
	     <blockquote>
	     <a href="http://www.w3.org/Addressing/">URL (Uniform Resource Locator)</a> - 
	     only URL's with prefix <code>http://</code> are supported (e.g., <code>http://www.it.edu</code>).
	     </blockquote>`

      fun msgEnum enums s =
            `^s
	     You must choose among the following enumerations:
	     <blockquote>
	     ^(String.concatWith "," enums)
	     </blockquote>`

      fun msgDateIso s = 
            `^s
	     <blockquote>
	     You must type a <b>date</b> in the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-10-25).
	     </blockquote>`

      fun msgDate s = 
            `^s
	     <blockquote>
	     You must type a <b>date</b> in either the Danish format <code>DD/MM-YYYY</code> (e.g., 25/01-2001) or 
	     the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-01-25).
	     </blockquote>`

      fun msgTableName s = 
            `^s
	     <blockquote>
	     You have not specified a valid <b>table name</b>
	     </blockquote>`

      fun chkEnum enums v =
	case List.find (fn enum => v = enum) enums
	  of NONE => false
	   | SOME _ => true
    in
      val getEmailErr = getErr' "email" msgEmail
	(fn email => regExpMatch "[^@\t ]+@[^@.\t ]+(\\.[^@.\n ]+)+" (trim email)) 
      val getNameErr = getErr' "name" msgName (regExpMatch "[a-zA-ZAÆØÅaæøå '\\-]+")
      val getAddrErr = getErr' "address" msgAddr (regExpMatch "[a-zA-Z0-9ÆØÅæøå '\\-.:;,]+")
      val getLoginErr = getErr' "login" msgLogin 
	(fn login =>
	 regExpMatch "[a-z][a-z0-9\\-]+" login andalso 
	 String.size login >= 3 andalso String.size login <= 10)
      val getPhoneErr = getErr' "phone number" msgPhone (regExpMatch "[a-zA-Z0-9ÆØÅæøå '\\-.:;,]+")
      (* getHtml : not implemented yet *)
      val getHtmlErr = getErr' "HTML text" msgHTML (fn html => html <> "")
      val getUrlErr =  getErr' "URL" msgURL (regExpMatch "http://[0-9a-zA-Z/\\-\\\\._~]+(:[0-9]+)?")
      val getEnumErr = fn enums => getErr' "enumeration" (msgEnum enums) (chkEnum enums)
      val getYesNoErr = let val enums = ["Yes","No"] in getErr' "Yes/No" (msgEnum enums) (chkEnum ["t","f"]) end
      val getTableName = getErr' "table name" msgTableName (regExpMatch "[a-zA-Z_]+")
    end

    fun getStrings fv = List.map trim (Web.Conn.formvarAll fv)
  end

