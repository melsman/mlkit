signature SCS_FORM_VAR =
  sig
    (* Checking form variables are very important but also 
       tedious, because the same kind of code is written 
       in almost every file. This module overcomes the 
       tedious part by defining several functions that can 
       be used to test form variables throughout a large
       system.

       The idea is that you define a list of form types, 
       corresponding to the values used in forms. For instance, 
       you may have an email type representing all possible
       email-values. For every form type, you define a
       function getFormtype, that takes three arguments, 
         (1) the name of the form-variable holding the value, 
         (2) the name of the field in the form; the user may 
	     be presented an errorpage with more than one 
	     error and it is important that the error message 
	     refer to a given field in the form
         (3) an error container of type errs used to hold 
	     the error messages sent back to the user.
       The type formvar_fn represents the type of 
       functions used to check form variables. The functions
       are named getFormtypeErr.

       When you have checked all the form values, you can 
       call the function any_errors, which returns an 
       error-page if any errors occurred and otherwise 
       proceeds with the remainder of the script. If an 
       error-page is returned, then the script is terminated.

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
      
(* to consider so that wrapXXX can be used on both these functions and
specialised ones. 

    type 'a formvar_fn = string * errs -> 'a * errs

    type field_name = ScsDict.dict

    val getIntErr      : msg -> int formvar_fn

val getUserIdErr = getIntErr "User id" 

val getUserIdErr : int formvar_fn 

    val wrapOpt : 'a formvar_fn -> (string -> 'a option)

    val wrapMaybe : 'a -> 'a formvar_msg_fn -> 'a formvar_msg_fn 

      type fv = string
      type 'a formvar

      val getInt : fv -> int formvar

      fejl
      val wrapOpt : 'a formvar -> 'a option
      val wrapExn : 'a formvar -> 'a
      val wrapErr : field_name -> 'a formvar -> errs -> 'a * errs

      hvornår er der fejl
      val wrapMaybe : 'a -> ('a formvar -> 'b) -> 'b

      ekstra information
      val wrapDate : 'a formvar -> date * 'a


datetype 'a formvar =
   OK of fv * 'a
 | ERROR of fv * string
 | MISSING of fv

val getUserIdMaybeErr = ((wrapMaybe 0 (wrapErr "User id")) o getInt) "user_id"

wrapMaybe wrapErr
wrapMaybe wrapOpt
wrapMaybe wrapExn




errs

val getUserIdErr : fv -> errs -> int * errs = wrapErr "User id" o getInt

val getUserIdErr = (wrapErr "User id" o getInt) "user_id"

val (user_id,errs) = getUserIdErr "user_id" errs

2003-02-23, nh, se eks i imp_row.sml
*)

    val emptyErr : errs
    val addErr : quot * errs -> errs
    val buildErrMsg : errs -> quot
    val anyErrors : errs -> unit
    val isErrors  : errs -> bool

    val getIntErr              : int formvar_fn
    val getNatErr              : int formvar_fn
    val getRealErr             : real formvar_fn
    val getStringErr           : string formvar_fn
    val getStringLenErr        : int -> string formvar_fn
    val getNonEmptyStringErr   : string formvar_fn
    val getNonEmptyStringLenErr: int -> string formvar_fn
    val getIntRangeErr         : int -> int -> int formvar_fn
    val getEmailErr            : string formvar_fn 
    val getNameErr             : string formvar_fn 
    val getAddrErr             : string formvar_fn
    val getLoginErr            : string formvar_fn
    val getPhoneErr            : string formvar_fn
    val getHtmlErr             : string formvar_fn
    val getUrlErr              : string formvar_fn
    val getCprErr              : string formvar_fn
    val getEnumErr             : string list -> string formvar_fn
    val getYesNoErr            : string formvar_fn
    val getDateErr             : Date.date formvar_fn
    val getDbTimestampErr      : Date.date formvar_fn
    val getTimestampErr        : Date.date formvar_fn
    val getPeriodErr           : (Date.date * Date.date) formvar_fn
    val getStartdateEndtimeErr : (Date.date * Date.date) formvar_fn
    val getDateIso             : string formvar_fn
    val getTableName           : string formvar_fn
    val getLangErr             : ScsLang.lang formvar_fn
    val getRegExpErr           : RegExp.regexp formvar_fn
    val getRoleIdErr           : string * errs -> int * errs

    val wrapQQ  : string formvar_fn -> (string * string) formvar_fn
    val wrapOpt : 'a formvar_fn -> (string -> 'a option)
    val wrapMaybe : 'a formvar_fn -> 'a formvar_fn
    val wrapMaybe_nh : 'a -> 'a formvar_fn -> 'a formvar_fn 
    val wrapExn : 'a formvar_fn -> (string -> 'a)
    val wrapFail : 'a formvar_fn -> (string * string -> 'a)
    val wrapPanic : (quot -> 'a) -> 'a formvar_fn -> (string -> 'a)
    val wrapIntAsString : int formvar_fn -> string formvar_fn

    val getStrings : string -> string list

    (* For extensions *)
    val getErr : 'a -> (string->'a) -> string -> (string->quot) -> (string->bool) -> 'a formvar_fn
  end

structure ScsFormVar :> SCS_FORM_VAR =
  struct
    val trim = ScsString.trim

    type quot = string frag list
    type errs = quot list
    type 'a formvar_fn = string * string * errs -> 'a * errs

    val regExpMatch   = RegExp.match   o RegExp.fromString
    val regExpExtract = RegExp.extract o RegExp.fromString

    exception FormVar of string

    val emptyErr : errs = []

    fun addErr (emsg:quot,errs:errs) = emsg :: errs
    fun genErrMsg (f_msg:string,msg:quot) : quot = 
       `^(ScsDict.s [(ScsLang.en, `Error in field`),(ScsLang.da, `Fejl i felt`)]) ^f_msg. ` ^^ msg
    fun errNoFormVar(f_msg:string,ty:string) : quot = 
      ScsDict.sl' [(ScsLang.en,`Error in field %0. You must provide a <b>%1</b>.`),
                     (ScsLang.da,`Fejl i felt %0. Du skal angive <b>%1</b>.`)] [f_msg, ty]
    fun errTypeMismatch(f_msg:string,ty:string,v:string) : quot = 
      ScsDict.sl' [(ScsLang.en,`Error in field %0. You must provide a <b>%1</b> - <i>%2</i> is not a %1.`),
                     (ScsLang.da,`Fejl i felt %0. Du skal angive <b>%1</b> - <i>%2</i> er ikke %1.`)] [f_msg,ty,v]
    fun errTooLarge(f_msg:string,ty:string,v:string) : quot =
      ScsDict.sl' [(ScsLang.en,`Error in field %0. The provided %1 (<i>%2</i>) is too large.`),
	           (ScsLang.da,`Fejl i felt %0. Det indtastede %1 (<i>%2</i>) er for stor.`)] [f_msg,ty,v]
    fun errTooMany(f_msg:string) : quot =
      ScsDict.sl' [(ScsLang.en,`Error in field %0. More than one dataitem is provided.`),
                   (ScsLang.da,`Fejl i felt %0. Der findes mere end en indtastning.`)] [f_msg]
    fun buildErrMsg (errs: errs) : quot =
      (case ScsLogin.user_lang of
	 ScsLang.en => `
	   We had a problem processing your entry:

	   <ul>` ^^ 
	   Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^ `
	   </ul>

	   Please back up using your browser, correct it, and resubmit your entry<p>
	   
	   Thank you.`
	 | ScsLang.da => 
	   let
	     val (problem_string, please_correct) = if List.length errs = 1 then
	       ("en fejl","fejlen") else ("nogle fejl","fejlene")
	   in
	     `Vi har fundet ^problem_string i dine indtastede data:
	     <ul>` ^^ 
	     Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^ `
	     </ul>

	     Vær venlig at klikke på "tilbage"-knappen i din browser, og ret
	     ^please_correct. Derefter kan du indsende dine oplysninger igen<p>
	     På forhånd tak.`
	   end)

    fun returnErrorPg (errs: errs) : Ns.status = 
      ScsPage.returnPg (ScsDict.s [(ScsLang.en,`Form Error`),(ScsLang.da,`Formularfejl`)]) (buildErrMsg errs)

    fun anyErrors ([]:errs) = ()
      | anyErrors (errs) = (returnErrorPg errs; Ns.exit())

    fun isErrors ([]:errs) = false
      | isErrors (errs) = true

    fun wrapQQ (f : string formvar_fn) : string * string * errs -> (string * string) * errs =
      fn arg =>
      case f arg of
	(v,e) => ((v,Db.qq v),e)

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
	| _ => ScsError.panic `ScsFormVar.wrapIntAsString failed on ^fv`)

    fun wrapMaybe (f : 'a formvar_fn) =
      (fn (fv,emsg,errs) => 
       (case Ns.Conn.formvarAll fv of
	  [] => (case f(fv,emsg,[]) of (v,_) => (v,errs)) (* No formvar => don't report error *)
	| [v] => 
	   (if trim v = "" then
	      (case f(fv,emsg,[]) of (v,_) => (v,errs)) (* Don't report error *)
	    else f(fv,emsg,errs))
	| _ => f(fv,emsg,errs))) (* Multiple formvars => report error *)

    fun wrapMaybe_nh empty_val (f : 'a formvar_fn) =
      (fn (fv,emsg,errs) => 
       (case Ns.Conn.formvarAll fv of
	  [] => (empty_val,errs) (* No formvar => don't report error *)
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
       | (_,errs) => (returnErrorPg errs; Ns.exit())

    fun wrapPanic (f_panic: quot -> 'a) (f : 'a formvar_fn) : string -> 'a =
      fn fv =>
      ((case f (fv,fv,[]) of
	(v,[]) => v
       | (_,x::xs) => f_panic(`^("\n") ^fv : ` ^^ x))
	  handle X => f_panic(`^("\n") ^fv : ^(General.exnMessage X)`))

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
      val getIntErr = getErrWithOverflow 0 (ScsDict.s [(ScsLang.en,`number`),(ScsLang.da,`tal`)])
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
      val getNatErr = getErrWithOverflow 0 (ScsDict.s [(ScsLang.en,`positive number`),(ScsLang.da,`positivt tal`)])
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
	  
      val getRealErr = getErrWithOverflow 0.0 (ScsDict.s [(ScsLang.en,`real`),(ScsLang.da,`kommatal`)])
	(fn v => let val l = explode (ScsReal.normReal v)
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

      val getStringErr = 
        getErrWithOverflow "" (ScsDict.s [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)])
          (fn v => if size v = 0 then NONE else SOME v)

      fun getStringLenErr l = getErrWithOverflow "" 
        (ScsDict.sl [(ScsLang.en,`string or it is too long - max. %0 characters`),
	             (ScsLang.da,`tekststreng eller den er for lang - maks %0 tegn`)] [Int.toString l])
	(fn v => if size v = 0 orelse size v > l then NONE else SOME v)
    end

    fun getIntRangeErr a b (args as (fv:string,emsg:string,errs:errs)) =
      let
	val (i,errs') = getIntErr args
      in
	if List.length errs = List.length errs' then
	  if a <= i andalso i <= b 
	    then (i,errs)
	  else (0,addErr(genErrMsg(emsg,
                                   ScsDict.sl' [(ScsLang.en,`The integer <i>%0</i> is not within the valid range [%1,...,%2].`),
                                                (ScsLang.da,`Tallet <i>%0</i> er ikke indenfor intervallet [%1,...,%2].`)]
                                   [Int.toString i,Int.toString a,Int.toString b]),errs))
	else
	  (0,errs')
      end
    
    fun getErr (empty_val:'a) (conv_val:string->'a) (ty:string) (add_fn:string->quot) (chk_fn:string->bool) =
      let
        val err1 = [(ScsLang.en,`You must provide a <b>%0</b>.`),
                    (ScsLang.da,`Du skal indtaste <b>%0</b>.`)]
        val err2 = [(ScsLang.en,`You must provide an valid <b>%0</b> - <i>%1</i> is not one`),
	            (ScsLang.da,`Du skal indtaste korrekt <b>%0</b> - <i>%1</i> er ikke korrekt`)]
      in
        fn (fv:string,emsg:string,errs:errs) =>
        case Ns.Conn.formvarAll fv of
          []  => (empty_val,addErr(genErrMsg(emsg,add_fn (ScsDict.sl err1 [ty])),errs))
        | [""]  => (empty_val,addErr(genErrMsg(emsg,add_fn (ScsDict.sl err1 [ty])),errs))
        | [v] => 
	    if chk_fn v then
	      (conv_val v,errs)
  	    else
	      (empty_val, addErr(genErrMsg(emsg,add_fn (ScsDict.sl err2 [ty,v])),errs))
        | _ => (empty_val, addErr(errTooMany emsg,errs))
      end

    local
      val getErr' = getErr "" trim
      fun msgString s =
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     You must type a string`
	| ScsLang.da => `^s
	     Du skal indtaste en tegnstreng.`)
      fun msgLenString l s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     The string must be on no more that ^(Int.toString l) characters.`
	| ScsLang.da => `^s
	     Strengen må ikke være på mere end ^(Int.toString l) tegn`)
      fun msgEmail s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>A few examples of valid emails:
	     <ul>
	     <li>login@it-c.dk
	     <li>user@supernet.com
	     <li>FirstLastname@very.big.company.com\n
	     </ul></blockquote>`
	| ScsLang.da => `^s
	     <blockquote>Her er nogle eksempler på emails:
	     <ul>
	     <li>login@it-c.dk
	     <li>user@supernet.com
	     <li>FirstLastname@very.big.company.com
	     </ul></blockquote>`)
      fun msgName s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     A name may contain the letters from the alphabet including: <b>'</b>, <b>\</b>,<b>-</b>,<b>æ</b>,
	     <b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>,<b>ü</b>,<b>ä</b>,<b>é</b>,<b>á</b> and space.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et navn må indeholde bogstaver fra alfabetet samt disse tegn: <b>'</b>, <b>\</b>,<b>-</b>,<b>æ</b>,
	     <b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>,<b>ü</b>,<b>ä</b>,<b>é</b>,<b>á</b> og mellemrum.
	     </blockquote>`)
      fun msgAddr s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     An address may contain digits, letters from the alphabet including:
	     <b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>,
	     <b>æ</b>,<b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>,<b>ü</b>,<b>á</b>
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     En adresse må indeholde tal, bogstaver fra alfabetet samt disse tegn: 
	     <b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>,
	     <b>æ</b>,<b>ø</b>,<b>å</b>,<b>Æ</b>,<b>Ø</b>,<b>Å</b>,<b>ü</b>,<b>á</b>
	     </blockquote>`)
      fun msgLogin s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     A login may contain lowercase letters from the alphabet and digits - the first
	     character must not be a digit. Special characters 
	     like <b>æ</b>,<b>ø</b>,<b>å</b>,<b>;</b>,<b>^^</b>,<b>%</b> are not alowed. 
	     A login must be no more than 10 characters and at least three characters.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et login må indeholde bogstaver fra alfabetet og tal - det første tegn må 
	     ikke være et tal. Specialtegn såsom <b>æ</b>,<b>ø</b>,<b>å</b>,<b>;</b>,
	     <b>^^</b>,<b>%</b> er ikke tilladt.
	     Derudover skal et login være på mindst tre tegn og højst 10 tegn.
	     </blockquote>`)
      fun msgPhone s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     A telephone numer may contain numbers and letters from the alphabet 
	     including <b>-</b>, <b>,</b> and <b>.</b>.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et telefonnummer må indeholde tal og bogstaver fra alfabetet
	     samt <b>-</b>, <b>,</b> and <b>.</b>.
	     </blockquote>`)
      fun msgHTML s = 	
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You may use the following HTML tags in your text: Not implemented yet.
	     </blokcquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Det er tilladt at anvende følgende HTML tags i din tekst: Desværre ikke implementeret.
	     </blokcquote>`)
      fun msgURL s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     <a href="/url_desc.sml">URL (Uniform Resource Locator)</a> - 
	     we only support the <code>http://</code> type (e.g., <code>http://www.it.edu</code>).
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     <a href="/url_desc.sml">URL (Uniform Resource Locator)</a> - 
	     vi supporterer kun <code>http://</code> type (f.eks. <code>http://www.it-c.dk</code>).
	     </blockquote>`)
      fun msgCpr s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
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
	 | ScsLang.da => `^s
	     <blockquote>
	     Hvis du har et dansk CPR-nummer, så er formatet:
	     DDMMYYYY-TTTT, hvor TTTT er fire tal, eksempelvis
	     291270-1234. <p>

	     Derudover udføres 
	     <a href="http://www.cpr.dk/modulus11_beregn.htm">modulus 11 check</a>.<p>

	     Hvis du ikke har et dansk CPR-nummer, skrives dato,
	     måned og år for din fødselsdag i den angivne
	     rækkefølge i de seks felter før stregen. I de
	     første 3 felter efter stregen skrives de to første
	     bogstaver i dit (første) fornavn efterfulgt af det
	     første bogstav i dit (sidste) efternavn. I den
	     sidste rubrik angives dit køn med 1 for mand og 2
	     for kvinde.  <p> 

	     En mand, uden dansk CPR-nummer,
	     ved navn Claes Anders Fredrik Moren, født den
	     31. august 1975, skal skrive: <b>310875-CLM1</b>.
	     </blockquote>`)
      fun msgEnum enums s =
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     You must choose among the following enumerations:
	     <blockquote>
	     ^(String.concatWith "," enums)
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en af de følgende værdier:
	     <blockquote>
	     ^(String.concatWith "," enums)
	     </blockquote>`)
      fun msgDateIso s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>date</b> in the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-10-25).
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en <b>dato</b> i ISO formatet, dvs. <code>YYYY-MM-DD</code> (f.eks. 2001-10-25).
	     </blockquote>`)
      fun msgDate s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>date</b> in either the Danish format <code>DD/MM-YYYY</code> (e.g., 25/01-2001) or 
	     the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-01-25).
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en <b>dato</b> enten i formatet <code>DD/MM-YYYY</code> (f.eks. 25/01-2001) eller
	     i formatet <code>YYYY-MM-DD</code> (f.eks. 2001-01-25).
	     </blockquote>`)

      fun msgDbTimestamp s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>time</b> and <b>date</b> in this format <code>YYYY-MM-DD HH:MM:SS</code>.
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en <b>tid</b> og <b>dato</b> i formatet <code>YYYY-MM-DD HH:MM:SS</code>.
	     </blockquote>`)

      fun msgTableName s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You have not specified a valid <b>table name</b>
	     </blockquote>`
	| ScsLang.da => `^s
	     Du har ikke specificeret et korrekt <b>tabelnavn</b>.
	     </blockquote>`)
      fun msgRegExp s = 
	(case ScsLogin.user_lang of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>regular expression</b> defined as follows.<p>
	     <pre>
 Grammar for regular expressions (RegExp):

    re ::= re1 "|" re2         re1 or re2
        |  re1 re2             re1 followed by re2
        |  re "*"              re repeated zero or more times
        |  re "+"              re repeated one or more times
        |  re "?"              re zero or one time
        |  "(" re ")"          re
        |  c                   specific character
        |  "\" c               escaped character; c is one of |,*,+,?,(,),[,],$,.,\,t,n,v,f,r
        |  "[" class "]"       character class
        |  "[^^" class "]"      negated character class
        |  $                   empty string
        |  .                   any character

    class ::=  c               specific character
           |   "\" c           escaped character; c is one of [,],-,\,t,n,v,f,r
           |   c1 "-" c2       ascii character range
           |                   empty class 
           |   class class     composition
            
 Whitespace is significant.  Special characters can be escaped by \  
</pre>
	     </blockquote>`
	| ScsLang.da => `^s
	     <blockquote>
	     Du skal indtaste et <b>regulært udtryk</b> defineret således.<p>
	     <pre>
 Grammatik for regulære udtryk (RegExp):

    re ::= re1 "|" re2         re1 eller re2
        |  re1 re2             re1 efterfulgt af re2
        |  re "*"              re gentaget nul eller flere gange
        |  re "+"              re gentaget en eller flere gange
        |  re "?"              re nul eller en gang
        |  "(" re ")"          re
        |  c                   angivet tegn c
        |  "\" c               escaped karakter; c er en af følgende: |,*,+,?,(,),[,],$,.,\,t,n,v,f,r
        |  "[" class "]"       tengklasse
        |  "[^^" class "]"      negeret tegnklasse
        |  $                   tom tegnstreng
        |  .                   ethvert tegn

    class ::=  c               angivet tegn
           |   "\" c           escaped tegn, c er en af følgende: [,],-,\,t,n,v,f,r
           |   c1 "-" c2       ascii tegn interval
           |                   tom klasse
           |   class class     sammensætning
            
 Mellemrum har betydning. Tegn escapes ved \  
</pre>
	     </blockquote>`)
      fun msgLang s = msgEnum (ScsLang.all_as_text ScsLogin.user_lang) s

      fun convCpr cpr =
	case String.explode (trim cpr) of
	  d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: (#"-") :: t1 :: t2 :: t3 :: t4 :: [] =>
	    String.implode[d1,d2,m1,m2,y1,y2,t1,t2,t3,t4]
	| d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: t1 :: t2 :: t3 :: t4 :: [] =>
	    String.implode[d1,d2,m1,m2,y1,y2,t1,t2,t3,t4]
	| _ => ScsError.panic `ScsFormVar.convCpr failned on ^cpr`
	  
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
		  if ScsDate.dateOk (dd,mm,yyyy) andalso chk_modulus11(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) then 
		    true
		  else
		    false
		end
	      else
		(* NON DK CPR no *)
		if Char.isAlpha t1 andalso Char.isAlpha t2 andalso Char.isAlpha t3 andalso
		  (t4 = (#"1") orelse t4 = (#"2")) andalso
		  ScsDate.dateOk (dd,mm,yyyy) then
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
      fun dateOk (d,m,y) = ScsDate.dateOk(Option.valOf (Int.fromString d),
					  Option.valOf (Int.fromString m),Option.valOf (Int.fromString y))
      fun timeOk (h,m,s) = ScsDate.timeOk(Option.valOf (Int.fromString h),
					  Option.valOf (Int.fromString m),Option.valOf (Int.fromString s))

      fun genDate (d,m,y) = ScsDate.genDate(Option.valOf (Int.fromString d),
					    Option.valOf (Int.fromString m),Option.valOf (Int.fromString y))
      fun genTimestamp(dd,mm,yyyy,h,m,s) = ScsDate.genTimestamp(Option.valOf (Int.fromString dd),
								Option.valOf (Int.fromString mm),
								Option.valOf (Int.fromString yyyy),
								Option.valOf (Int.fromString h),
								Option.valOf (Int.fromString m),
								Option.valOf (Int.fromString s))

      fun chkDateIso v =
	(case regExpExtract "([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)" v of
	   SOME [yyyy,mm,dd] => dateOk(dd,mm,yyyy)
	 | _ => (case regExpExtract "([0-9][0-9][0-9][0-9])([0-9][0-9]?)([0-9][0-9]?)" v of
		   SOME [yyyy,mm,dd] => dateOk(dd,mm,yyyy)
		 | _ => false))
	   handle _ => false      
      fun chkDate v =
	(case regExpExtract "([0-9][0-9]?)/([0-9][0-9]?)-([0-9][0-9][0-9][0-9])" v of
	   SOME [dd,mm,yyyy] => dateOk(dd,mm,yyyy)
	 | _ => (case regExpExtract "([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)" v of
		   SOME [yyyy,mm,dd] => dateOk(dd,mm,yyyy)
		 | _ => false))
	   handle _ => false   
      fun convDate v =
	(case regExpExtract "([0-9][0-9]?)/([0-9][0-9]?)-([0-9][0-9][0-9][0-9])" v of
	   SOME [dd,mm,yyyy] => genDate(dd,mm,yyyy)
	 | _ => (case regExpExtract "([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)" v of
		   SOME [yyyy,mm,dd] => genDate(dd,mm,yyyy)
		 | _ => ScsError.panic `ScsFormVar.convDate failed on ^v`))
	   handle _ => ScsError.panic `ScsFormVar.convDate failed on ^v`

      fun chkDbTimestamp v =
	(case regExpExtract 
	   "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9]) ([0-9][0-9]):([0-9][0-9]):([0-9][0-9]).*" v of
	   SOME [yyyy,mm,dd,h,m,s] => dateOk(dd,mm,yyyy) andalso timeOk(h,m,s)
	 | _ => false)
	   handle _ => false   

      fun convDbTimestamp v =
	(case 
	   regExpExtract "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9]) ([0-9][0-9]):([0-9][0-9]):([0-9][0-9]).*" v of
	   SOME [yyyy,mm,dd,h,m,s] => genTimestamp(dd,mm,yyyy,h,m,s)
	   | _ => ScsError.panic `ScsFormVar.convTimestamp failed on ^v`)
	   handle _ =>  ScsError.panic `ScsFormVar.convTimestamp failed on ^v`

      fun chkRegExp v = (RegExp.fromString v; true) handle _ => false
      fun chkLang v = (ScsLang.fromString v; true) handle _ => false
    in
      val getNonEmptyStringErr = 
	getErr' (ScsDict.s [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)]) msgString
	(fn str => trim str <> "")

      fun getNonEmptyStringLenErr l = 
	getErr' (ScsDict.s [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)]) (msgLenString l)
	(fn str => let val str' = trim str in str' <> "" andalso (size str') <= l end)

      val getEmailErr = getErr' (ScsDict.s [(ScsLang.en,`email`),(ScsLang.da,`email`)]) msgEmail
	(fn email => regExpMatch "[^@\t ]+@[^@.\t ]+(\\.[^@.\n ]+)+" (trim email)) 
      val getNameErr = getErr' (ScsDict.s [(ScsLang.en,`name`),(ScsLang.da,`navn`)]) 
                         msgName (regExpMatch "[a-zA-ZAÆØÅaæøåüäéá '\\-]+")
      val getAddrErr = getErr' (ScsDict.s [(ScsLang.en, `address`),(ScsLang.da, `adresse`)])
                         msgAddr (regExpMatch "[a-zA-Z0-9ÆØÅæøåüá '\\-.:;,]+")
      val getLoginErr = getErr' (ScsDict.s [(ScsLang.en, `login`),(ScsLang.da,`login`)]) msgLogin 
	(fn login =>
	 regExpMatch "[a-z][a-z0-9\\-]+" login andalso 
	 String.size login >= 2 andalso String.size login <= 10)
      val getPhoneErr = getErr' (ScsDict.s [(ScsLang.en, `phone number`),(ScsLang.da,`telefonnummer`)]) 
                          msgPhone (regExpMatch "[a-zA-Z0-9ÆØÅæøå '\\-.:;,]+")
      (* getHtml : not implemented yet *)
      val getHtmlErr = getErr' (ScsDict.s [(ScsLang.en, `HTML text`),(ScsLang.da,`HTML tekst`)]) 
                          msgHTML (fn html => html <> "")
      val getUrlErr =  getErr' (ScsDict.s [(ScsLang.en,`URL`),(ScsLang.da,`URL`)]) 
                          msgURL (regExpMatch "http://[0-9a-zA-Z/\\-\\\\._~]+(:[0-9]+)?")
      val getCprErr = getErr "" convCpr (ScsDict.s [(ScsLang.en,`cpr number`),(ScsLang.da,`cpr nummer`)]) msgCpr chkCpr
      val getEnumErr = fn enums => getErr' (ScsDict.s [(ScsLang.en,`enumeration`),(ScsLang.da,`enumerering`)]) 
                                     (msgEnum enums) (chkEnum enums)
      val getYesNoErr = let val enums = [ScsDict.s [(ScsLang.en,`Yes`),(ScsLang.da,`Ja`)],
                                         ScsDict.s [(ScsLang.en,`No`),(ScsLang.da, `Nej`)]] 
                         in getErr' (ScsDict.s [(ScsLang.en,`Yes/No`),(ScsLang.da,`Ja/Nej`)]) 
                              (msgEnum enums) (chkEnum ["t","f"]) 
                        end
      val getDateIso = getErr' (ScsDict.s [(ScsLang.en,`date`),(ScsLang.da,`dato`)]) msgDateIso chkDateIso
      val getDateErr = getErr (ScsDate.genDate(1,1,1)) convDate (ScsDict.s [(ScsLang.en,`date`),(ScsLang.da,`dato`)]) 
                         msgDate chkDate
      val getDbTimestampErr = getErr (ScsDate.genTimestamp(1,1,1,0,0,0)) 
	convDbTimestamp (ScsDict.s [(ScsLang.da,`dato og tidspunt`),(ScsLang.en,`time and date`)])
	msgDbTimestamp chkDbTimestamp
				      
      fun getPeriodErr (fv:string,emsg:string,errs:errs) = 
        (* we append fv with _FV_start__ and _FV_end__ because we actually
           have two separate input boxes, see function period in ScsWidget. *)
        let
          val (start_date,errs) = 
            getDateErr(fv^"_FV_start__",ScsDict.s [(ScsLang.en,`start date of period`),
						   (ScsLang.da,`start dato for periode`)],errs) 
          val (end_date,errs) =
            getDateErr(fv^"_FV_end__",ScsDict.s [(ScsLang.en,`end date of period`),
						 (ScsLang.da,`slutdato for periode`)],errs)
          val errs = 
            if Date.compare(start_date,end_date) = General.GREATER then
              addErr(ScsDict.sl' [(ScsLang.en,`The start date <i>%0</i> must be before the end date <i>%1</i>.`),
                                  (ScsLang.da,`Startdatoen <i>%0</i> skal ligge før slutdatoen <i>%1</i>.`)] 
                                 [ScsDate.pp start_date,ScsDate.pp end_date],errs)
            else errs
        in
          ((start_date,end_date),errs)
        end

      fun getTimestampErr (fv:string,emsg:string,errs:errs) = 
        (* we append fv with _FV_date__, _FV_min__ and _FV_hour__
           because we actually have three separate input boxes, see
           function intextTimestamp in ScsWidget. *)
        let
	  fun mk_date_fv fv = fv^"_FV_date__"
	  fun mk_hour_fv fv = fv^"_FV_hour__"
	  fun mk_min_fv fv = fv^"_FV_min__"
          val (date,errs) = 
            getDateErr(mk_date_fv fv,
		       ScsDict.s [(ScsLang.en,`date part of timestamp`),
				  (ScsLang.da,`dato for tidspunkt`)],errs) 
          val (hour,errs) =
            getIntRangeErr 0 23 (mk_hour_fv fv,
				 ScsDict.s [(ScsLang.en,`time part of timestamp`),
					    (ScsLang.da,`time del af tidspunkt`)],errs)
          val (min,errs) =
            getIntRangeErr 0 59 (mk_min_fv fv,
				 ScsDict.s [(ScsLang.en,`minute part of timestamp`),
					    (ScsLang.da,`minut del af tidspunkt`)],errs)
        in
          (ScsDate.genTimestamp (Date.day date,
				 ScsDate.mthFromName(Date.month date),
				 Date.year date,
				 hour,
				 min,
				 0),errs)
        end

      fun getStartdateEndtimeErr (fv:string,emsg:string,errs:errs) = 
        (* we append fv with _FV_start__ and _FV_end__ because we
           actually have two separate input boxes, see function
           startdateEndtime in ScsWidget. *)
        let
          val (start_date,errs) = 
            getDateErr(fv^"_FV_start__",ScsDict.s [(ScsLang.en,`start date of period`),
						   (ScsLang.da,`start dato for periode`)],errs) 
          val (end_time,errs) =
            getTimestampErr(fv^"_FV_end__",ScsDict.s [(ScsLang.en,`end date and time of period`),
						 (ScsLang.da,`slut dato og tidspunkt for periode`)],errs)
          val errs = 
            if Date.compare(start_date,end_time) = General.GREATER then
              addErr(ScsDict.sl' [(ScsLang.en,`The start date <i>%0</i> must be before 
				   the end date and time <i>%1</i>.`),
                                  (ScsLang.da,`Startdatoen <i>%0</i> skal ligge før slutdatoen <i>%1</i>.`)] 
                                 [ScsDate.pp start_date,ScsDate.ppTimestamp end_time],errs)
            else errs
        in
          ((start_date,end_time),errs)
        end

      val getTableName = getErr' (ScsDict.s [(ScsLang.da,`table name`),(ScsLang.en,`tabelnavn`)]) 
                           msgTableName (regExpMatch "[a-zA-Z_]+")
      val getLangErr = getErr ScsLang.en ScsLang.fromString (ScsDict.s [(ScsLang.en,`language`),(ScsLang.da,`sprog`)])
                         msgLang chkLang
      val getRegExpErr = getErr (RegExp.fromString "$") RegExp.fromString (ScsDict.s [(ScsLang.en,`regular expression`),
                                                                                      (ScsLang.da,`regulaert udtryk`)])
                           msgRegExp chkRegExp
    end

    fun getStrings fv = List.map trim (Ns.Conn.formvarAll fv)



    fun getRoleIdErr (fv,errs) = getIntErr(fv,ScsDict.s [(ScsLang.en,`Role id`),(ScsLang.da,`Rolle id`)],errs)

  end

