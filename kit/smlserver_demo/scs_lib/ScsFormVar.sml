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

    val getIntErr      : msg -> int formvar_fnrr

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

      hvorn�r er der fejl
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
    val mergeErrs : errs * errs -> errs

    val getIntErr              : int formvar_fn
    val getIntTrimErr          : int formvar_fn
    val getNatErr              : int formvar_fn
    val getRealErr             : real formvar_fn
    val getPosRealErr          : real formvar_fn
    val getStringErr           : string formvar_fn
    val getStringLenErr        : int -> string formvar_fn
    val getStringMinLenErr     : int -> string formvar_fn
    val getStringWithinListErr : string list -> string formvar_fn
    val getNonEmptyStringErr   : string formvar_fn
    val getNonEmptyStringLenErr: int -> string formvar_fn
    val getIntRangeErr         : int -> int -> int formvar_fn
    val getRealRangeErr        : real -> real -> real formvar_fn
    val getEmailErr            : string formvar_fn 
    val getNameErr             : string formvar_fn 
    val getAddrErr             : string formvar_fn
    val getLoginErr            : string formvar_fn
    val getLoginOrEmailErr     : string formvar_fn
    val getPhoneErr            : string formvar_fn
    val getHtmlErr             : string formvar_fn
    val getUrlErr              : string formvar_fn
    val getCprErr              : string formvar_fn
    val getISSNErr             : string formvar_fn
    val getISBNErr             : string formvar_fn
    val getEnumErr             : string list -> string formvar_fn
    val getEnumsErr            : string list -> string list formvar_fn
    val getYesNoErr            : string formvar_fn
    val getMthErr              : int formvar_fn
    val getWeekErr              : int formvar_fn
    val getYearErr             : int formvar_fn
    val getDateErr             : Date.date formvar_fn
    val getWeekdayErr          : Date.weekday formvar_fn
    val getDbTimestampErr      : Date.date formvar_fn
    val getTimestampErr        : Date.date formvar_fn
    val getPeriodErr           : (Date.date * Date.date) formvar_fn
    val getStartdateEndtimeErr : (Date.date * Date.date) formvar_fn
    val getDateIso             : string formvar_fn
    val getTableName           : string formvar_fn
    val getLangErr             : ScsLang.lang formvar_fn
    val getRegExpErr           : RegExp.regexp formvar_fn
    val getRoleIdErr           : string * errs -> int * errs
    val getBoolErr	       : bool formvar_fn

    val wrapQQ  : string formvar_fn -> (string * string) formvar_fn
    val wrapOpt : 'a formvar_fn -> (string -> 'a option)
    val wrapMaybe : 'a formvar_fn -> 'a formvar_fn
    val wrapMaybeOpt : 'a formvar_fn -> 'a option formvar_fn 
    val wrapMaybe_nh : 'a -> 'a formvar_fn -> 'a formvar_fn 
    val wrapExn : 'a formvar_fn -> (string -> 'a)
    val wrapFail : 'a formvar_fn -> (string * string -> 'a)
    val wrapPanic : (quot -> 'a) -> 'a formvar_fn -> (string -> 'a)
    val wrapIntAsString : int formvar_fn -> string formvar_fn

    val getStrings : string -> string list

    (* For extensions *)
    val getErr : 'a -> (string->'a) -> ScsDict.dict -> (string->quot) 
      -> (string->bool) -> 'a formvar_fn
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

    fun getStrings fv = List.map trim (Ns.Conn.formvarAll fv)

    fun addErr (emsg:quot,errs:errs) = emsg :: errs
    fun genErrMsg (f_msg:string,msg:quot) : quot = 
       `^(ScsDict.s [(ScsLang.en, `Field:`),(ScsLang.da, `Felt:`)]) <b>^(f_msg)</b>. ` ^^ msg
    fun errNoFormVar(f_msg:string,ty:string) : quot = 
      ScsDict.sl' [(ScsLang.en,`Field: <b>%0</b>. You must provide a <b>%1</b>.`),
                     (ScsLang.da,`Felt <b>%0</b>. Du skal angive <b>%1</b>.`)] [f_msg, ty]
    fun errTypeMismatch(f_msg:string,ty:string,v:string) : quot = 
      ScsDict.sl' [(ScsLang.en,`Field: <b>%0</b>. You must provide a <b>%1</b> - <i>%2</i> is not a %1.`),
                     (ScsLang.da,`Felt: <b>%0</b>. Du skal angive <b>%1</b> - <i>%2</i> er ikke %1.`)] [f_msg,ty,v]
    fun errTooLarge(f_msg:string,ty:string,v:string) : quot =
      ScsDict.sl' [(ScsLang.en,`Field: <b>%0</b>. The provided %1 (<i>%2</i>) is too large.`),
	           (ScsLang.da,`Felt: <b>%0</b>. Det indtastede %1 (<i>%2</i>) er for stor.`)] [f_msg,ty,v]
    fun errTooMany(f_msg:string) : quot =
      ScsDict.sl' [(ScsLang.en,`Field: <b>%0</b>. More than one dataitem is provided.`),
                   (ScsLang.da,`Felt: <b>%0</b>. Der findes mere end en indtastning.`)] [f_msg]
    fun buildErrMsg (errs: errs) : quot =
      case ScsLogin.user_lang() of
	 ScsLang.en => 
	   (`There were the following errors:

	    <ul>` ^^ 
	    Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^ `
	    </ul>

	    ^(UcsPage.history_link "Go back") and correct the errors.`)
	 | ScsLang.da => 
	     (`Der var f�lgende fejl i de indtastede data:
	      <ul>` ^^ 
	      Quot.concatFn (fn q => `<li>` ^^ q) (List.rev errs) ^^ `
	      </ul>

	      ^(UcsPage.history_link "G� tilbage") og ret fejlene.`)

    fun returnErrorPg (errs: errs) : Ns.status = 
      ScsPage.returnPg (ScsDict.s [(ScsLang.en,`Form Error`),(ScsLang.da,`Formularfejl`)]) (buildErrMsg errs)

    fun anyErrors ([]:errs) = ()
      | anyErrors (errs) = (returnErrorPg errs; Ns.exit())

    fun isErrors ([]:errs) = false
      | isErrors (errs) = true

    fun mergeErrs (e1,e2) = e1 @ e2

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

    fun wrapMaybeOpt (f : 'a formvar_fn) = (
      fn (fv,emsg,errs) => (
        case Ns.Conn.formvarAll fv of
	    [] => (NONE, errs) (* No formvar => don't report error *)
	  | [v] => 
	     (if trim v = "" then
		(NONE, errs) (* Don't report error *)
	      else let val (v,errs) = f(fv,emsg,errs) in (SOME v,errs) end
	     )
	  | _ => let val (v,errs) = f(fv,emsg,errs) in (SOME v,errs) end 
		 (* Multiple formvars => report error *)
      )
    )

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
      fun getErrWithOverflow (empty_val:'a) (ty:ScsDict.dict) (chk_fn:string->'a option) =
	fn (fv:string,emsg:string,errs:errs) =>
	(case Ns.Conn.formvarAll fv of
	   [] => (empty_val,addErr(errNoFormVar(emsg,ScsDict.s ty),errs))
	 | [""] => (empty_val,addErr(errNoFormVar(emsg,ScsDict.s ty),errs))
	 | [v] => ( 
	   (case chk_fn v of
		SOME v => (v,errs)
	      | NONE => (empty_val, addErr(errTypeMismatch(emsg,ScsDict.s ty,v),errs)) 
	   )
	   handle Overflow => 
	       (empty_val, addErr(errTooLarge(emsg,ScsDict.s ty,v),errs) )
         )
	 | _ => (empty_val, addErr(errTooMany emsg,errs)) )
    in
      val getIntErr = getErrWithOverflow 0 [(ScsLang.en,`number`),(ScsLang.da,`tal`)]
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
      val getIntTrimErr = getErrWithOverflow 0 [(ScsLang.en,`number`),(ScsLang.da,`tal`)]
	(fn v => let val l = (explode o ScsString.trim) v
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

      val getNatErr = getErrWithOverflow 0 [(ScsLang.en,`positive number`),(ScsLang.da,`positivt tal`)]
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
	  
      val getRealErr = getErrWithOverflow 0.0 [(ScsLang.en,`real`),(ScsLang.da,`kommatal`)]
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

      val getPosRealErr = getErrWithOverflow 0.0 [(ScsLang.en,` positive real`),(ScsLang.da,`positivt kommatal`)]
	(fn v => let val l = explode (ScsReal.normReal v)
		 in
		   case l
		     of c::_ => 
		       if Char.isDigit c then
			 (case Real.scan List.getItem l
			    of SOME (n, nil) => SOME n
			  | _ => NONE)
		       else NONE
		   | nil => NONE
		 end)

      val getStringErr = 
        getErrWithOverflow "" [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)]
          (fn v => if size v = 0 then NONE else SOME v)

      fun getStringWithinListErr l = 
        getErrWithOverflow "" [(ScsLang.en,`string within the following: [^(String.concatWith ", " l)]`),(ScsLang.da,`tekststreng iblandt f�lgende: [^(String.concatWith ", " l)]`)]
          (fn v => if not(List.exists (fn str=>str=v) l) then NONE else SOME v)

      fun getStringLenErr l = getErrWithOverflow "" 
        (ScsDict.dictWithArgsToDict 
	 [(ScsLang.en,`string or it is too long - max. %0 characters`),
	  (ScsLang.da,`tekststreng eller den er for lang - maks %0 tegn`)] [Int.toString l])
	(fn v => if size v = 0 orelse size v > l then NONE else SOME v)

      fun getStringMinLenErr l = getErrWithOverflow "" 
        (ScsDict.dictWithArgsToDict 
	 [(ScsLang.en,`string or it is too short - min. %0 characters`),
	  (ScsLang.da,`tekststreng eller den er for kort - minimum %0 tegn`)] [Int.toString l])
	(fn v => if size v = 0 orelse size v < l then NONE else SOME v)

      val getWeekdayErr = getErrWithOverflow Date.Mon [(ScsLang.da,`Ugedag`),(ScsLang.en,`Day of Week`)] (fn str => ScsDate.weekday_from_DB str)


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

    fun getRealRangeErr a b (args as (fv:string,emsg:string,errs:errs)) =
      let
	val (x,errs') = getRealErr args
      in
	if List.length errs = List.length errs' then
	  if a <= x andalso x <= b 
	    then (x,errs)
	  else (0.0,addErr(genErrMsg(emsg,
                                   ScsDict.sl' [(ScsLang.en,`The real <i>%0</i> is not within the valid range [%1,...,%2].`),
                                                (ScsLang.da,`Tallet <i>%0</i> er ikke indenfor intervallet [%1,...,%2].`)]
                                   [Real.toString x,Real.toString a,Real.toString b]),errs))
	else
	  (0.0,errs')
      end

    fun getErr (empty_val:'a) (conv_val:string->'a) (ty_dict:ScsDict.dict) 
               (add_fn:string->quot) (chk_fn:string->bool) =
      let
        val err1 = [(ScsLang.en,`You must provide a <b>%0</b>.`),
                    (ScsLang.da,`Du skal indtaste <b>%0</b>.`)]
        val err2 = [(ScsLang.en,`You must provide an valid <b>%0</b> - <i>%1</i> is not one`),
	            (ScsLang.da,`Du skal indtaste korrekt <b>%0</b> - <i>%1</i> er ikke korrekt`)]
      in
        fn (fv:string,emsg:string,errs:errs) =>
	let
	  val ty = ScsDict.s ty_dict
	in
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
      end

    local
      val getErr' = getErr "" trim
      fun msgString s = `^s` 
      fun msgLenString l s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     The string must be on no more that ^(Int.toString l) characters.`
	| ScsLang.da => `^s
	     Strengen m� ikke v�re p� mere end ^(Int.toString l) tegn`)
      fun msgEmail s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>A few examples of valid emails:
	     <ul>
	     <li>login@itu.dk
	     <li>user@supernet.com
	     <li>FirstLastname@very.big.company.com\n
	     </ul></blockquote>`
	| ScsLang.da => `^s
	     <blockquote>Her er nogle eksempler p� emails:
	     <ul>
	     <li>login@itu.dk
	     <li>user@supernet.com
	     <li>FirstLastname@very.big.company.com
	     </ul></blockquote>`)
      fun msgName s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     A name may contain the letters from the alphabet including: <b>'</b>, <b>\</b>,
	     <b>-</b>,<b>�</b>,
	     <b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,
	     <b>�</b>,<b>�</b> and space.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et navn m� indeholde bogstaver fra alfabetet samt disse tegn: <b>'</b>, <b>\</b>,
	     <b>-</b>,<b>�</b>,
	     <b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,
	     <b>�</b>, <b>�</b> og mellemrum.
	     </blockquote>`)
      fun msgAddr s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     An address may contain digits, letters from the alphabet including:
	     <b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>,
	     <b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     En adresse m� indeholde tal, bogstaver fra alfabetet samt disse tegn: 
	     <b>'</b>, <b>\\ </b>, <b>-</b>, <b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>,
	     <b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>,<b>�</b>
	     </blockquote>`)
      fun msgLogin s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     A login may contain lowercase letters from the alphabet and digits - the first
	     character must not be a digit. Special characters 
	     like <b>�</b>,<b>�</b>,<b>�</b>,<b>;</b>,<b>^^</b>,<b>%</b> are not alowed. 
	     A login must be no more than 10 characters and at least three characters.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et login m� indeholde <b>sm�</b> bogstaver fra alfabetet og tal - det 
	     f�rste tegn m� 
	     ikke v�re et tal. Specialtegn s�som <b>�</b>,<b>�</b>,<b>�</b>,<b>;</b>,
	     <b>^^</b>,<b>%</b> er ikke tilladt.
	     Derudover skal et login v�re p� mindst tre tegn og h�jst 10 tegn.
	     </blockquote>`)
      fun msgPhone s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     A telephone numer may contain numbers and letters from the alphabet 
	     including <b>-</b>, <b>,</b> and <b>.</b>.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Et telefonnummer m� indeholde tal og bogstaver fra alfabetet
	     samt <b>-</b>, <b>,</b> and <b>.</b>.
	     </blockquote>`)
      fun msgHTML s = 	
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     You may use the following HTML tags in your text: Not implemented yet.
	     </blokcquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Det er tilladt at anvende f�lgende HTML tags i din tekst: Desv�rre ikke implementeret.
	     </blokcquote>`)
      fun msgURL s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     <a href="/url_desc.sml">URL (Uniform Resource Locator)</a> - 
	     we only support the <code>http://</code> and <code>https://</code> 
             types (e.g., <code>http://www.itu.dk</code> and <code>https://www.itu.dk</code>).<p>
	     The maximum length is 200 characters.
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     <a href="/url_desc.sml">URL (Uniform Resource Locator)</a> - 
	     vi supporterer kun typerne <code>http://</code> og <code>https://</code>
	     (f.eks. <code>http://www.itu.dk</code> og <code>https://www.itu.dk</code>).<p>
             Den maksimale l�ngde er 200 karakterer.
	     </blockquote>`)
      fun msgCpr s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     If you hold a Danish CPR-number, then the format is:
	     <code>DDMMYY-TTTT</code>, where <code>TTTT</code> are four numbers, for instance
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
	     Hvis du har et dansk CPR-nummer, s� er formatet:
	     DDMMYY-TTTT, hvor TTTT er fire tal, eksempelvis
	     291270-1234. <p>

	     Derudover udf�res 
	     <a href="http://www.cpr.dk/modulus11_beregn.htm">modulus 11 check</a>.<p>

	     Hvis du ikke har et dansk CPR-nummer, skrives dato,
	     m�ned og �r for din f�dselsdag i den angivne
	     r�kkef�lge i de seks felter f�r stregen. I de
	     f�rste 3 felter efter stregen skrives de to f�rste
	     bogstaver i dit (f�rste) fornavn efterfulgt af det
	     f�rste bogstav i dit (sidste) efternavn. I den
	     sidste rubrik angives dit k�n med 1 for mand og 2
	     for kvinde.  <p> 

	     En mand, uden dansk CPR-nummer,
	     ved navn Claes Anders Fredrik Moren, f�dt den
	     31. august 1975, skal skrive: <b>310875-CLM1</b>.
	     </blockquote>`)
      fun msgISSN s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The format of a ISSN-number is
	     <code>DDDD-DDDT</code>, where <code>D</code> is a number and <code>T</code> 
	     is either a number or the capital letter X. This is a valid ISSN number: <code>1602-3536</code>. <p>
	  
	     We also perform a modulus 11 check of the number you type in.<p>
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Formatet af et ISSN-nummer er
	     <code>DDDD-DDDT</code>, hvor <code>D</code> is a tal og <code>T</code> 
	     er et tal eller det store bogstav X. Et lovligt ISSN-nummer er eksempelvis: <code>1602-3536</code>. <p>

	     Vi udf�rer ogs� et modulus 11 check p� det ISSN nummer som du indtaster.<p>
	     </blockquote>`)

      fun msgISBN s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The format of a ISBN-number is
	     <code>DD-DDDD-DDD-T</code>, where <code>D</code> is a number and <code>T</code> 
	     is either a number or the capital letter X. This is a valid ISBN number: <code>87-7949-044-1</code>. <p>
	  
	     We also perform a modulus 11 check of the number you type in.<p>
	     </blockquote>`
	 | ScsLang.da => `^s
	     <blockquote>
	     Formatet af et ISBN-nummer er
	     <code>DD-DDDD-DDD-T</code>, hvor <code>D</code> is a tal og <code>T</code> 
	     er et tal eller det store bogstav X. Et lovligt ISBN-nummer er eksempelvis: <code>87-7949-044-1</code>. <p>

	     Vi udf�rer ogs� et modulus 11 check p� det ISBN nummer, som du indtaster.<p>
	     </blockquote>`)

      fun msgEnum enums s =
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     You must choose among the following enumerations:
	     <blockquote>
	     ^(String.concatWith "," enums)
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en af de f�lgende v�rdier:
	     <blockquote>
	     ^(String.concatWith "," enums)
	     </blockquote>`)
      fun msgDateIso s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>date</b> in the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-10-25).
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en <b>dato</b> i ISO formatet, dvs. <code>YYYY-MM-DD</code> (f.eks. 2001-10-25).
	     </blockquote>`)
      fun msgDate s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The date must be in the Danish format <code>DD/MM-YYYY</code> (e.g., 25/01-2001), in the format <code>DD-MM-YYYY</code> (e.g., 28-12-1972) or 
	     in the ISO format <code>YYYY-MM-DD</code> (e.g., 2001-01-25). The date must be after 1900.
	     </blockquote>`
	| ScsLang.da => `^s
	     Datoen skal v�re i formatet <code>DD/MM-YYYY</code> (f.eks. 25/01-2001), i formatet <code>DD-MM-YYYY</code> (f.eks. 28-12-1972) eller
	     i formatet <code>YYYY-MM-DD</code> (f.eks. 2001-01-25). Datoen skal v�re efter 1900
	     </blockquote>`)
      fun msgMth s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The month must be between 1 and 12
	     </blockquote>`
	| ScsLang.da => `^s
	     M�ned skal v�re i intervallet 1 til 12.
	     </blockquote>`)
      fun msgWeek s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The week must be between 1 and 53
	     </blockquote>`
	| ScsLang.da => `^s
	     Uge skal v�re i intervallet 1 til 53.
	     </blockquote>`)

      fun msgYear s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     The year must be after 1900.
	     </blockquote>`
	| ScsLang.da => `^s
	     �r skal v�re efter 1900.
	     </blockquote>`)

      fun msgDbTimestamp s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     You must type a <b>time</b> and <b>date</b> in this format <code>YYYY-MM-DD HH:MM:SS</code>.
             The year must be after 1900.
	     </blockquote>`
	| ScsLang.da => `^s
	     Du skal indtaste en <b>tid</b> og <b>dato</b> i formatet <code>YYYY-MM-DD HH:MM:SS</code>.
             �ret skal v�re efter 1900.
	     </blockquote>`)

      fun msgTableName s = 
	(case ScsLogin.user_lang() of
	   ScsLang.en => `^s
	     <blockquote>
	     You have not specified a valid <b>table name</b>
	     </blockquote>`
	| ScsLang.da => `^s
	     Du har ikke specificeret et korrekt <b>tabelnavn</b>.
	     </blockquote>`)
      fun msgRegExp s = 
	(case ScsLogin.user_lang() of
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
	     Du skal indtaste et <b>regul�rt udtryk</b> defineret s�ledes.<p>
	     <pre>
 Grammatik for regul�re udtryk (RegExp):

    re ::= re1 "|" re2         re1 eller re2
        |  re1 re2             re1 efterfulgt af re2
        |  re "*"              re gentaget nul eller flere gange
        |  re "+"              re gentaget en eller flere gange
        |  re "?"              re nul eller en gang
        |  "(" re ")"          re
        |  c                   angivet tegn c
        |  "\" c               escaped karakter; c er en af f�lgende: |,*,+,?,(,),[,],$,.,\,t,n,v,f,r
        |  "[" class "]"       tengklasse
        |  "[^^" class "]"      negeret tegnklasse
        |  $                   tom tegnstreng
        |  .                   ethvert tegn

    class ::=  c               angivet tegn
           |   "\" c           escaped tegn, c er en af f�lgende: [,],-,\,t,n,v,f,r
           |   c1 "-" c2       ascii tegn interval
           |                   tom klasse
           |   class class     sammens�tning
            
 Mellemrum har betydning. Tegn escapes ved \  
</pre>
	     </blockquote>`)
      fun msgLang s = msgEnum (ScsLang.all_as_text (ScsLogin.user_lang())) s

      fun msgBool s = `^s`
(*	if ScsString.trim s <> "" then
	  ( case ScsLogin.user_lang() of
	      ScsLang.en => `^s
		<blockquote>
		This is not a <b>boolean</b> value
		</blockquote>`
	    | ScsLang.da => `^s
		Dette er ikke en <b>sand/falsk</b> v�rdi.
		</blockquote>` )
	else
	  ``*)

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

	(* Calculating the check digit
	 Excerpted from the ISSN Manual, a publication of the ISSN International Network 

	 The purpose of a check digit is to guard against errors
	 caused by the incorrect transcription of an ISSN. The modulus
	 11 basis using the weighting factors 8 to 2 for calculating
	 the check digit is one of the most efficient systems for
	 detecting transcription errors.

         The procedure for calculating the check digit, which may be carried out 
	 automatically in a computer, is as follows: 
  
         -  Take the first seven digits of the ISSN (the check digit is the 
	    eighth and last digit): 0 3 1 7 8 4 7 
         -  Take the weighting factors associated with each digit : 8 7 6 5 4 3 2 
         -  Multiply each digit in turn by its weighting factor: 0 21 6 35 32 12 14 
         -  Add these numbers together: 0+21+6+35+32+12+14 = 120 
         - Divide this sum by the modulus 11: 120:11 =10 remainder 10 
         - Substract the remainder from 11: 11-10 = 1 
         - Add the remainder, which is the check digit, to the extreme right 
	  (low order) position of the base number of the ISSN: 0317-8471 

	  If the remainder is 10, substitute an upper case X in the
	  check digit position. If there is no remainder, put a zero
	  in the check digit position.

	  It should be noted that the check digit is an essential and
	  inseparable part of the ISSN.*)
      fun chkISSN issn =
	let
	  fun chk_modulus11 (d1,d2,d3,d4,d5,d6,d7,c1) =
	    let
	      val sum1 = d1*8 + d2*7 + d3*6 + d4*5 + d5*4 + d6*3 + d7*2
	      val check_number = Int.mod(sum1,11)
	      val remainder = 11 - check_number 
	    in
	      (remainder = 10 andalso c1 = "X") orelse
	      (Int.toString remainder = c1)
	    end
	  fun issn_ok (d1,d2,d3,d4,d5,d6,d7,c1) =
	    let
	      fun c2d ch = Option.valOf(Int.fromString(String.implode [ch]))
	    in
	      chk_modulus11(c2d d1,c2d d2,c2d d3,c2d d4,c2d d5,c2d d6,c2d d7,c1)
	    end
	in
	  case String.explode (trim issn) of
	    d1 :: d2 :: d3 :: d4 :: (#"-") :: d5 :: d6 :: d7 :: c1 :: [] =>
	      issn_ok(d1,d2,d3,d4,d5,d6,d7,Char.toString c1)
	  | 	    d1 :: d2 :: d3 :: d4  :: d5 :: d6 :: d7 :: c1 :: [] =>
	      issn_ok(d1,d2,d3,d4,d5,d6,d7,Char.toString c1)
	  | _ => false
	end
      handle _ => false

      fun chkISBN isbn =
	let
	  fun chk_modulus11 (d1,d2,d3,d4,d5,d6,d7,d8,d9,c1) =
	    let
	      val sum1 = d1*10 + d2*9 + d3*8 + d4*7 + d5*6 + d6*5 + d7*4 + d8*3 + d9*2
	      val check_number = Int.mod(sum1,11)
	      val remainder = 11 - check_number 
	    in
	      (remainder = 10 andalso c1 = "X") orelse
	      (Int.toString remainder = c1)
	    end
	  fun isbn_ok (d1,d2,d3,d4,d5,d6,d7,d8,d9,c1) =
	    let
	      fun c2d ch = Option.valOf(Int.fromString(String.implode [ch]))
	    in
	      chk_modulus11(c2d d1,c2d d2,c2d d3,c2d d4,c2d d5,c2d d6,c2d d7,c2d d8,c2d d9,c1)
	    end
	in
	  case String.explode (trim isbn) of
	    d1 :: d2 :: (#"-") :: d3 :: d4 :: d5 :: d6 :: (#"-") :: d7 :: d8 :: d9 :: (#"-") :: c1 :: [] =>
	      isbn_ok(d1,d2,d3,d4,d5,d6,d7,d8,d9,Char.toString c1)
	  | d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: d8 :: d9 :: c1 :: [] =>
	      isbn_ok(d1,d2,d3,d4,d5,d6,d7,d8,d9,Char.toString c1)
	  | _ => false
	end
      handle _ => false

      fun convISSN issn =
	case String.explode (trim issn) of
	  d1 :: d2 :: d3 :: d4 :: (#"-") :: d5 :: d6 :: d7 :: c1 :: [] =>
	    String.implode[d1,d2,d3,d4,(#"-"),d5,d6,d7,c1]
	| d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: c1 :: [] =>
	    String.implode[d1,d2,d3,d4,(#"-"),d5,d6,d7,c1]
	| _ => ScsError.panic `ScsFormVar.convISSN failned on ^issn`
	  
      fun convISBN isbn =
	case String.explode (trim isbn) of
	  d1 :: d2 :: (#"-") :: d3 :: d4 :: d5 :: d6 :: (#"-") :: d7 :: d8 :: d9 :: (#"-") :: c1 :: [] =>
	    String.implode[d1,d2,(#"-"),d3,d4,d5,d6,(#"-"),d7,d8,d9,(#"-"),c1]
	| 	  d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: d7 :: d8 :: d9 :: c1 :: [] =>
	    String.implode[d1,d2,(#"-"),d3,d4,d5,d6,(#"-"),d7,d8,d9,(#"-"),c1]
	| _ => ScsError.panic `ScsFormVar.convISBN failned on ^isbn`

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

      val isoDatePat1 = "([0-9][0-9][0-9][0-9])-([0-9][0-9]?)-([0-9][0-9]?)" 
      val isoDatePat2 = "([0-9][0-9][0-9][0-9])([0-9][0-9]?)([0-9][0-9]?)"

      fun chkDateIso v =
	(case regExpExtract isoDatePat1 v of
	   SOME [yyyy,mm,dd] => dateOk(dd,mm,yyyy)
	 | _ => (case regExpExtract isoDatePat2 v of
		   SOME [yyyy,mm,dd] => dateOk(dd,mm,yyyy)
		 | _ => false))
	   handle _ => false      

      val datePat1 = Quot.toString `([0-9][0-9]?)[/\-]([0-9][0-9]?)[/\-]([0-9][0-9][0-9][0-9])`
(* 2005-04-19, knp: OBSOLETE
      val datePat2 = "([0-9][0-9]?)-([0-9][0-9]?)-([0-9][0-9][0-9][0-9])"
*)
      fun chkMth v =
	case regExpExtract "([0-9][0-9]?)" v of
	  SOME [m] => 
	    let
	      val m = ScsError.valOf (Int.fromString m)
	    in
	      if m >= 1 andalso m <= 12 then
		true
	      else
		false
	    end
	| _ => false

      fun chkWeek v =
	case regExpExtract "([0-9][0-9]?)" v of
	  SOME [m] => 
	    let
	      val m = ScsError.valOf (Int.fromString m)
	    in
	      if m >= 1 andalso m <= 53 then
		true
	      else
		false
	    end
	| _ => false

      fun chkYear v =
	case regExpExtract "([0-9][0-9][0-9][0-9])" v of
	  SOME [y] => if (ScsError.valOf o Int.fromString) y > 1900 then true else false
	| _ => false

      fun chkDate v = ( case regExpExtract datePat1 v of
	  SOME [dd,mm,yyyy] => dateOk(dd,mm,yyyy)
	| _		    => chkDateIso v 
      )
      handle _ => false   

      fun convDate v = (case regExpExtract datePat1 v of
	  SOME [dd,mm,yyyy] => genDate(dd,mm,yyyy)
	| _		    => (case regExpExtract isoDatePat1 v of
	    SOME [yyyy,mm,dd] => genDate(dd,mm,yyyy)
	  | _		        => ( 
	    ScsError.panic `ScsFormVar.convDate failed on ^v`
	  )
	)
      )
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
	getErr' [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)] msgString
	(fn str => trim str <> "")
      fun getNonEmptyStringLenErr l = 
	getErr' [(ScsLang.en,`string`),(ScsLang.da,`tekststreng`)] (msgLenString l)
	(fn str => let val str' = trim str in str' <> "" andalso (size str') <= l end)

      val getEmailErr = getErr' [(ScsLang.en,`email`),(ScsLang.da,`email`)] msgEmail
	(fn email => regExpMatch "[^@\t ]+@[^@.\t ]+(\\.[^@.\n ]+)+" (trim email)) 

      val getNameErr = getErr' [(ScsLang.en,`name`),(ScsLang.da,`navn`)] 
                         msgName (regExpMatch "[a-zA-ZA���a�������� '\\-]+")

      val getAddrErr = getErr' [(ScsLang.en, `address`),(ScsLang.da, `adresse`)]
                         msgAddr (regExpMatch "[a-zA-Z0-9��������� '\\-.:;,/]+")

      val getLoginErr = getErr' [(ScsLang.en, `login`),(ScsLang.da,`login`)] msgLogin 
	(fn login =>
	 regExpMatch "[a-z][a-z0-9\\-]+" login andalso 
	 String.size login >= 2 andalso String.size login <= 15)

      fun getLoginOrEmailErr (fv,field_name,errs) =
	let
	  val err_msg_dict =
	    [(ScsLang.da,`Fejl i felt ^(field_name). Du skal indtaste en email. `),
	     (ScsLang.en,`Error in field ^(field_name). You must type in an email.`)]
        in
	  case wrapOpt getEmailErr fv of
	    NONE => 
	      (case wrapOpt getLoginErr fv of
		 NONE => ("",addErr(ScsDict.s' err_msg_dict,errs))
	       | SOME l => (ScsPersonData.fix_email l,errs))
	  | SOME e => (ScsPersonData.fix_email e,errs)
	end
      val getPhoneErr = getErr' [(ScsLang.en, `phone number`),(ScsLang.da,`telefonnummer`)] 
                          msgPhone (regExpMatch "[a-zA-Z0-9������ '\\-.:;,]+")

      (* getHtml : not implemented yet *)
      val getHtmlErr = getErr' [(ScsLang.en, `HTML text`),(ScsLang.da,`HTML tekst`)] 
                          msgHTML (fn html => html <> "")
      val getUrlErr =  getErr' [(ScsLang.en,`URL`),(ScsLang.da,`URL`)] 
                          msgURL (fn url => 
				  regExpMatch "http(s?)://[0-9a-zA-Z/\\-\\\\._~]+(:[0-9]+)?" url andalso 
				  String.size url <= 200)

      val getCprErr = getErr "" convCpr [(ScsLang.en,`cpr number`),(ScsLang.da,`cpr nummer`)] msgCpr chkCpr
      val getISSNErr = getErr "" convISSN [(ScsLang.en,`ISSN number`),(ScsLang.da,`ISSN nummer`)] 
			      msgISSN chkISSN
      val getISBNErr = getErr "" convISBN [(ScsLang.en,`ISBN number`),(ScsLang.da,`ISBN nummer`)] 
			      msgISBN chkISBN
      val getEnumErr = fn enums => getErr' [(ScsLang.en,`enumeration`),(ScsLang.da,`enumerering`)] 
                                     (msgEnum enums) (chkEnum enums)
      fun getEnumsErr enums (fv,title,errs) =
	let
	  val sels = getStrings fv
	  val errs = 
	    if List.null sels then
	      addErr(ScsDict.s' [(ScsLang.da,`<b>^title</b>: Du skal angive mindst et valg`),
				 (ScsLang.en,`<b>^title</b>: You must make atleast one choise.`)],errs)
	    else
	      errs
	  fun chkEnumErr (enum,(acc,errs)) =
	    if chkEnum enums enum then 
	      (enum::acc,errs)
	    else 
	      (acc,addErr(msgEnum enums title,errs))
	in
	  List.foldr chkEnumErr ([],errs) sels
	end
      fun getYesNoErr args =
	let val enums = [ScsDict.s [(ScsLang.en,`Yes`),(ScsLang.da,`Ja`)],
			 ScsDict.s [(ScsLang.en,`No`),(ScsLang.da, `Nej`)]] 
	in getErr' [(ScsLang.en,`Yes/No`),(ScsLang.da,`Ja/Nej`)] 
	  (msgEnum enums) (chkEnum ["t","f"]) args
	end
      val getDateIso = getErr' [(ScsLang.en,`date`),(ScsLang.da,`dato`)] msgDateIso chkDateIso

      val getMthErr = getErr 1 (ScsError.valOf o Int.fromString) [(ScsLang.da,`m�ned`),(ScsLang.en,`month`)] msgMth chkMth
      val getWeekErr = getErr 1 (ScsError.valOf o Int.fromString) [(ScsLang.da,`uge`),(ScsLang.en,`week`)] msgWeek chkWeek

      val getYearErr = getErr 1 (ScsError.valOf o Int.fromString) [(ScsLang.da,`�r`),(ScsLang.en,`year`)] msgYear chkYear

      val getDateErr = getErr (ScsDate.genDate(1,1,1)) convDate [(ScsLang.en,`date`),(ScsLang.da,`dato`)] 
                         msgDate chkDate

      val getDbTimestampErr = getErr (ScsDate.genTimestamp(1,1,1,0,0,0)) 
	convDbTimestamp [(ScsLang.da,`dato og tidspunt`),(ScsLang.en,`time and date`)]
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
                                  (ScsLang.da,`Startdatoen <i>%0</i> skal ligge f�r slutdatoen <i>%1</i>.`)] 
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
                                  (ScsLang.da,`Startdatoen <i>%0</i> skal ligge f�r slutdatoen <i>%1</i>.`)] 
                                 [ScsDate.pp start_date,ScsDate.ppTimestamp end_time],errs)
            else errs
        in
          ((start_date,end_time),errs)
        end

      val getTableName = getErr' [(ScsLang.da,`table name`),(ScsLang.en,`tabelnavn`)] 
                           msgTableName (regExpMatch "[a-zA-Z_]+")
      val getLangErr = getErr ScsLang.en ScsLang.fromString [(ScsLang.en,`language`),(ScsLang.da,`sprog`)]
                         msgLang chkLang
      val getRegExpErr = getErr (RegExp.fromString "$") RegExp.fromString [(ScsLang.en,`regular expression`),
									   (ScsLang.da,`regulaert udtryk`)]
                           msgRegExp chkRegExp

      val getBoolErr = getErr true (valOf o Db.toBool) 
        [(ScsLang.da, `boolsk v�rdi`),(ScsLang.en, `boolean value`)] msgBool 
	(fn str => if str = "t" then true 
	           else if str = "f" then true 
		   else false)

    end (* of local block *)

    fun getRoleIdErr (fv,errs) = getIntErr(fv,ScsDict.s [(ScsLang.en,`Role id`),(ScsLang.da,`Rolle id`)],errs)




  end
