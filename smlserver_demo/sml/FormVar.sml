signature FORMVAR =
  sig
    type quot = string frag list
    type errs = quot list
    type 'a formvar_fn = string * string * errs -> 'a * errs

    val emptyErr : errs
    val getIntErr : int formvar_fn
    val getNatErr : int formvar_fn
    val getRealErr : real formvar_fn
    val getStringErr : string formvar_fn
    val getIntRangeErr : int -> int -> int formvar_fn

    val wrapOpt : 'a formvar_fn -> (string -> 'a option)
    val wrapExn : 'a formvar_fn -> (string -> 'a)
    val wrapFail : 'a formvar_fn -> (string * string -> 'a)

    val anyErrors : errs -> unit
  end

structure FormVar : FORMVAR =
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

    fun getIntErr (fv:string,emsg:string,errs:errs) = 
      case Ns.Conn.formvarAll fv of
	[] => (0,addErr(errNoFormVar(emsg,"number"),errs))
	| [s] =>
	  let 
	    val l = explode s
	  in 
	    case l
	      of c::_ => 
		if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		  ((case Int.scan StringCvt.DEC List.getItem l
		      of SOME (n, nil) => (n,errs)
		    | _ => (0,addErr(errTypeMismatch(emsg,"number",s),errs)))
			handle Overflow => (0, addErr(errTooLarge(emsg,"number",s),errs)))
		else (0,addErr(errTypeMismatch(emsg,"number",s),errs))
	   | nil => (0,addErr(errNoFormVar(emsg,"number"),errs))
	  end
      | _ => (0,addErr(errTooMany(emsg),errs))

    fun getNatErr (fv:string,emsg:string,errs:errs) =
      case Ns.Conn.formvarAll fv of 
	[] => (0,addErr(errNoFormVar(emsg,"positive number"),errs))
      | [s] => 
	  let 
	    val l = explode s
	  in 
	    case l
	      of c::_ => 
		if Char.isDigit c then
		  ((case Int.scan StringCvt.DEC List.getItem l
		      of SOME (n, nil) => (n,errs)
		    | _ => (0,addErr(errTypeMismatch(emsg,"positive integer",s),errs)))
		       handle Overflow => (0, addErr(errTooLarge(emsg,"positive integer",s),errs)))
		else (0,addErr(errTypeMismatch(emsg,"positive integer",s),errs))
	    | nil => (0,addErr(errNoFormVar(emsg,"positive integer"),errs))
	  end
      | _ => (0,addErr(errTooMany emsg,errs))

    fun getRealErr (fv: string,emsg:string,errs:errs) =
      case Ns.Conn.formvarAll fv of 
	[] => (0.0,addErr(errNoFormVar(emsg,"real"),errs))
      | [s] => 
	  let 
	    val l = explode s
	  in
	    case l
	      of c::_ => 
		if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		  ((case Real.scan List.getItem l
		      of SOME (n, nil) => (n,errs)
		    | _ => (0.0,addErr(errTypeMismatch(emsg,"real",s),errs)))
		       handle Overflow => (0.0,addErr(errTooLarge(emsg, "real", s),errs)))
		else (0.0,addErr(errTypeMismatch(emsg, "real",s),errs))
	    | nil => (0.0,addErr(errNoFormVar(emsg,"real"),errs))
	  end
      | _ => (0.0,addErr(errTooMany emsg,errs))

    fun getStringErr (fv:string,emsg:string,errs:errs) =
      case Ns.Conn.formvarAll fv of
	[] => ("",addErr(errNoFormVar(emsg,"string"), errs))
      | [s] => if size s = 0 then ("",addErr(errNoFormVar(emsg,"string"), errs))
	       else (s,errs)
      | _ => ("",addErr(errTooMany emsg,errs))

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
  end

(*
(* 

Checking form variables are very important but also tedious, because
the same kind of code is written in almost every file. This module
overcomes the problem by defining several functions which can be used
directly to test the form variables used throughout the system.

The idea is that you define a list of \emph{form types}, corresponding
to the values used in forms. For instance, you may have an email type
representing all the email-values. For every form type, you define a
function chk_formtype, that takes three arguments, (1) the name of the
form-variable holding the value, (2) an optional error message to
display if the value is mal formed and (3) an error container of type
\emph{form_error} used to hold the error messages sent back to the
user.

When you have checked all the form values, then you can call the
function any_errors, which will return an error-page if any errors
happended and otherwise do nothing. If an error-page is returned, then
we terminate the process.

The implemented types are:
   NAME        DESCRIPTION                     TYPE OF ERROR
   email                                       user input
   name                                        user input
   exists      the formvar must exists but no
               checking is done on the content user input
   url         A URL, for now only http is 
               supported                       user input
   uint        un-signed integer               user input
   range       a range [a--b]                  user input
   enum        one in a list                   user input
   dateDDMMYY  date in short format            user input
   seq_id      a postive integer usen in db    PANIC
*)

structure Formvar =
  struct

open Msp;
infix &&
val sl2wseq = prmap (fn s => $ s)
val sl2wseq_nl = prmap (fn s => ($ s) && Nl)
fun myahrefa link attr seq = 
    $$["<A HREF=\"", link, "\" ", attr, ">"] && seq && $"</A>"

fun regex rex str = RegExp.regexecBoolS rex str
fun regex2 rex str = RegExp.regexecLS rex str
fun trim_str str = String.implode (List.filter (not o Char.isSpace) (String.explode str))
fun trim_str_fb str =
  let
    fun drop_space(ch :: rest) = if Char.isSpace ch then drop_space rest else (ch::rest)
      | drop_space []= []
  in
    String.implode(List.rev(drop_space(List.rev(drop_space(String.explode str)))))
  end

type form_error = wseq list
val empty_error : form_error = []

fun any_errors (return_pg:string -> wseq -> unit) ([]:form_error) = ()
  | any_errors return_pg ers =
  let
    val (problem_string, please_correct) =
      if List.length ers = 1 then
	("en fejl","fejlen")
      else
	("nogle fejl","fejlene")
  in
    return_pg "Fejl i de indtastede data"
	      (($$ ["Vi har fundet ", problem_string, " i dine indtastede data:\n"]) &&
	       (ul (prmap li (List.rev ers))) &&
	       (sl2wseq_nl ["Vær venlig at klikke på \"tilbage\"-knappen i din browser, og ret ",
			    please_correct ^ ". ", 
			    "Derefter kan du indsende dine oplysninger igen.",
			    "<p>",
			    "På forhånd tak."]));
    Ns.exit()
  end

fun gen_err_msg (fvar_text, msg) = $"Fejl i " && $fvar_text && $". " && msg
fun add_err_msg (msg,errs) = $msg :: errs

fun chk_email form_var fvar_text errs =
  let
    val add = 
      sl2wseq_nl ["<blockquote>Her er nogle eksempler på emails:",
		  "<ul>",
		  "<li>login@it-c.dk",
		  "<li>user@supernet.com",
		  "<li>FirstLastname@very.big.company.com\n",
		  "</ul></blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste en <b>email</b> adresse." && add) :: errs)
    | [email] => 
	if Mail.check_email email then
	  (email,errs)
	else
	  ("", gen_err_msg(fvar_text,$$["Du skal indtaste en korrekt <b>email</b> adresse - '",
					email,
					"' er ikke korrekt"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end en <b>email</b> adresse?" && add) :: errs)
  end

fun chk_email_opt form_var fvar_text errs =
  let
    val add = 
      sl2wseq_nl ["<blockquote>Her er nogle eksempler på emails:",
		  "<ul>",
		  "<li>login@it-c.dk",
		  "<li>user@supernet.com",
		  "<li>FirstLastname@very.big.compagny.com\n",
		  "</ul></blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", errs)
    | [email] => 
	if trim_str email = "" orelse Mail.check_email (trim_str email) then
	  (trim_str email,errs)
	else
	  ("", gen_err_msg(fvar_text,$$["Du skal indtaste en korrekt <b>email</b> adresse - '",
					email,
					"' er ikke korrekt"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end en <b>email</b> adresse?" && add) :: errs)
  end

fun chk_name form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et navn må indeholde bogstaver fra alfabetet ",
		  "samt disse tegn: <b>'</b>, <b>\\ </b> og <b>-</b>.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste et <b>navn</b>." && add) :: errs)
    | [name] => 
	if regex "^.+$" (*"^[a-zA-ZA0-9ÆØÅaæøå '\\-]+$" RegExp-buggy 2001-05-31, Niels *) (trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>navn</b> - `",
					      trim_str_fb name,
					      " er ikke et korrekt navn"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>navn</b>?" && add) :: errs)
  end

fun chk_name_opt form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et navn må indeholde bogstaver fra alfabetet ",
		  "samt disse tegn: <b>'</b>, <b>\\ </b> og <b>-</b>.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", errs)
    | [name] => 
	if regex "^[a-zA-ZA0-9ÆØÅaæøå '\\-]*$" (trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>navn</b> - `",
					      trim_str_fb name,
					      " er ikke et korrekt navn"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>navn</b>?" && add) :: errs)
  end

fun chk_addr form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "En adresse må indeholde tal, bogstaver fra alfabetet ",
		  "samt disse tegn: <b>'</b>, <b>\\ </b>, <b>-</b>, ",
		  "<b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste en <b>adresse</b>." && add) :: errs)
    | [name] => 
	if regex "^.+$" (*"^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]+$" regexp seems buggy with æøåÅØÆ 2001-05-31, Niels *)(trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste en <b>adresse</b> - `",
					      trim_str_fb name,
					      " er ikke en korrekt adresse"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end en <b>adresse</b>?" && add) :: errs)
  end

fun chk_addr_opt form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "En adresse må indeholde tal, bogstaver fra alfabetet ",
		  "samt disse tegn: <b>'</b>, <b>\\ </b>, <b>-</b>, ",
		  "<b>.</b>, <b>:</b> og <b>;</b> og <b>,</b>",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", errs)
    | [name] => 
	if regex "^.*$" (*"^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]*$" 2001-05-31, Niels *) (trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste en <b>adresse</b> - `",
					      trim_str_fb name,
					      " er ikke en korrekt adresse"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end en <b>adresse</b>?" && add) :: errs)
  end

fun chk_login form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et login (som også anvendes som den første del af din email adresse) ",
		  "må indeholde bogstaver fra alfabetet. Dog er æ,ø og å, ",
		  "samt specialtegn <b>ikke</b> tilladt.",
		  "Derudover må et login kun være på 10 tegn.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste et <b>login</b>." && add) :: errs)
    | [name] => 
	if regex "^[a-zA-Z]+$" (trim_str_fb name) then
	  if String.size(trim_str_fb name) > 10 then
	    ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>login</b> på kun 10 tegn- `",
						trim_str_fb name,
						" er på ",
						(Int.toString(String.size(trim_str_fb name))),
						" tegn."] && add) :: errs)
	  else
	    (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>login</b> - `",
					      trim_str_fb name,
					      " er ikke et korrekt login"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>login</b>?" && add) :: errs)
  end


fun chk_phone form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et telefonnummer må indeholde tal og bogstaver fra alfabetet ",
		  "samt <b>-</b>.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste et <b>telefonnummer</b>." && add) :: errs)
    | [name] => 
	if regex "^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]+$" (trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>telefonnummer</b> - `",
					      trim_str_fb name,
					      " er ikke et korrekt telefonnummer"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>telefonnummer</b>?" && add) :: errs)
  end

fun chk_phone_opt form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et telefonnummer må indeholde tal og bogstaver fra alfabetet ",
		  "samt <b>-</b>.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", errs)
    | [name] => 
	if regex "^[a-zA-Z0-9ÆØÅæøå '\\-\\.:;,]*$" (trim_str_fb name) then
	  (trim_str_fb name, errs)
	else
	  ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>telefonnummer</b> - `",
					      trim_str_fb name,
					      " er ikke et korrekt telefonnummer"] && add) :: errs)
    | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>telefonnummer</b>?" && add) :: errs)
  end

fun chk_exists form_var fvar_text errs =
  case Mosmlcgi.cgi_field_strings form_var of 
    []  => ("", gen_err_msg(fvar_text,$"Du skal udfylde feltet.") :: errs)
    | [fvar] => 
      let
	val fvar' = trim_str_fb fvar
      in
	if fvar' <> "" then
	  (fvar', errs)
	else
	  ("", gen_err_msg(fvar_text,$"Du skal udfylde feltet.") :: errs)
      end
    | _ => ("", gen_err_msg(fvar_text, sl2wseq ["Der er mere end en formvariabel '",
						form_var,
						"?"]) :: errs)

(* No really check done here, yet *)
(* should check that it seems like HTML and that <html>,<title>,<body> tags are not used *)
(* Think about <textarea> *)
fun chk_HTML form_var fvar_text errs =
  case Mosmlcgi.cgi_field_strings form_var of 
    []  => ("", gen_err_msg(fvar_text,$"Du skal udfylde feltet.") :: errs)
  | [fvar] => 
      let val _ = Ns.log (Ns.Notice, "chk_HTML: " ^ fvar)
	val fvar' = trim_str_fb fvar
      in
	if fvar' <> "" then
	  (fvar', errs)
	else
	  ("", gen_err_msg(fvar_text,$"Du skal udfylde feltet.") :: errs)
      end
    | _ => ("", gen_err_msg(fvar_text, sl2wseq ["Der er mere end en formvariabel '",
						form_var,
						"?"]) :: errs)

fun chk_url form_var fvar_text errs =
  let
    val url_desc = myahrefa "url_desc_dk.cgi" "" ($"URL (<i>eng.</i> Uniform Resource Locator)")
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste en <b>" && url_desc && $"</b>.") :: errs)
    | [url] => 
	if regex "^http://([-0-9a-zA-Z/\\\\\\._]+)$" url then
	  (url, errs)
	else
	  ("", gen_err_msg(fvar_text, $"Du skal indtaste en <b>" && url_desc && $"</b> - '" &&
			   $url &&
			   $"' er ikke en url.") :: errs)
    | _ => ("", gen_err_msg(fvar_text, sl2wseq ["Der er mere end en formvariabel '",
						form_var,
						"?"]) :: errs)
  end

fun chk_uint form_var fvar_text errs =
  (case Mosmlcgi.cgi_field_strings form_var of
     [] => (0, gen_err_msg(fvar_text, $"Du skal indtaste et <b>positivt heltal</b>") :: errs)
   | [uint] =>
       ((case Int.fromString (trim_str_fb uint) of
	   NONE => (0, gen_err_msg(fvar_text,sl2wseq["Du skal indtaste et <b>positivt heltal</b> - '",
						     uint,
						     "' er ikke et positivt heltal"]) :: errs)
	 | SOME uint' => 
	     if uint' >= 0 then
	       (uint', errs)
	     else
	       (0, gen_err_msg(fvar_text,sl2wseq["Det indtastede tal '",
						 uint,
						 " er ikke positivt"]) :: errs))
	   handle Overflow => (0, gen_err_msg(fvar_text,sl2wseq ["Det indtastede tal '",
								 uint,
								 " er ikke gyldigt."]) :: errs))
   | _ => (0, gen_err_msg(fvar_text,sl2wseq ["Der er mere end en formvariabel '",
					     form_var,
					     "?"]) :: errs))

fun chk_int form_var fvar_text errs =
  (case Mosmlcgi.cgi_field_strings form_var of
     [] => (0, gen_err_msg(fvar_text,$"Du skal indtaste et <b>heltal</b>") :: errs)
   | [uint] =>
       ((case Int.fromString uint of
	   NONE => (0, gen_err_msg(fvar_text,sl2wseq["Du skal indtaste et <b>heltal</b> - '",
						     uint,
						     "' er ikke et heltal"]) :: errs)
	 | SOME uint' => (uint', errs))
	   handle Overflow => (0, gen_err_msg(fvar_text,sl2wseq ["Det indtastede tal '",
								 uint,
								 " er ikke gyldigt"]) :: errs))
   | _ => (0, gen_err_msg(fvar_text,sl2wseq ["Der er mere end en formvariabel '",
					     form_var,
					     "?"]) :: errs))

fun chk_range min max form_var fvar_text errs =
  let
    val (fvar,errs') = chk_int form_var fvar_text errs
  in
    if errs <> errs' then
      (0,errs')
    else
      if min <= fvar andalso fvar <= max then
	(fvar, errs)
      else
	(0, gen_err_msg(fvar_text,sl2wseq ["Tallet ",
					   Int.toString fvar,
					   " er ikke i intervallet [",
					   Int.toString min,
					   "..",
					   Int.toString max,
					   "]."]) :: errs)
  end

fun chk_cpr form_var fvar_text errs =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Hvis du har et dansk CPR-nummer, så er formatet: ",
		  "DDMMYYYY-TTTT, hvor TTTT er fire tal, eksempelvis ",
		  "291270-1234. <p>Derudover udføres <a href=\"http://www.cpr.dk/modulus11_beregn.htm\">modulus 11 check</a>. ",
		  "<p>",
		  "Hvis du ikke har et dansk CPR-nummer, skrives dato,",
                  "måned og år for din fødselsdag i den angivne",
                  "rækkefølge i de seks felter før stregen. I de",
                  "første 3 felter efter stregen skrives de to første",
                  "bogstaver i dit (første) fornavn efterfulgt af det",
                  "første bogstav i dit (sidste) efternavn. I den",
                  "sidste rubrik angives dit køn med 1 for mand og 2",
                  "for kvinde.  <p> En mand, uden dansk CPR-nummer, ",
                  "ved navn Claes Anders Fredrik Moren, født den",
                  "31. august 1975, skal skrive: <b>310875-CLM1</b>.",
		  "</blockquote>"]

    fun mk_yyyymmdd (d1,d2,m1,m2,y1,y2) =
      let
	val yy = Option.valOf(Int.fromString(String.implode [y1,y2]))
	val mm = Option.valOf(Int.fromString(String.implode [m1,m2]))
	val dd = Option.valOf(Int.fromString(String.implode [d1,d2]))
      in
	if yy < 50 then
	  (2000+yy,mm,dd)
	else
	  (1900+yy,mm,dd)
      end

   (* fun chk_modulus11 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) =
      let
	val sum1 = c1*4 + c2*3 + c3*2 + c4*7 + c5*6 + c6*5 + c7*4 + c8*3 + c9*2
	val remainder = Int.mod(sum1,11)
      in
	if remainder = 1 then
	  false 
	else
	  let
	    val kontrol_ciffer = 
	      if remainder = 0 then 
		0 
	      else 
		11 - remainder
	    val sum2 = c1*4 + c2*3 + c3*2 + c4*7 + c5*6 + c6*5 + 
	      c7*4 + c8*3 + c9*2 + kontrol_ciffer*1
	  in
	    Int.mod(sum2,11) = 0
	  end
      end 2001-06-07, Niels *)

    fun chk_modulus11 (c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) =
      let
	val sum1 = c1*4 + c2*3 + c3*2 + c4*7 + c5*6 + c6*5 + c7*4 + c8*3 + c9*2 + c10*1
      in
	Int.mod(sum1,11) = 0
      end

    fun cpr_ok (d1,d2,m1,m2,y1,y2,t1,t2,t3,t4) err_msg =
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
	    if ItcDate.date_ok dd mm yyyy andalso chk_modulus11(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) then 
	      (cpr, errs)
	    else
	      ("", err_msg :: errs)
	  end
	else
	  (* NON DK CPR no *)
	  if Char.isAlpha t1 andalso Char.isAlpha t2 andalso Char.isAlpha t3 andalso
	    (t4 = (#"1") orelse t4 = (#"2")) andalso
	     ItcDate.date_ok dd mm yyyy then
	     (cpr, errs)
	  else
	     ("", err_msg :: errs)
      end
  in
    (case Mosmlcgi.cgi_field_strings form_var of 
       []  => ("", gen_err_msg(fvar_text,$"Du skal indtaste et <b>CPR-nummer</b>." && add) :: errs)
     | [name] => 
	 let
	   val err_msg = gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste et <b>cpr-nummer</b> - `",
							name,
							"' er ikke et korrekt cpr-nummer."] && add)
	 in
	   case String.explode (trim_str_fb name) of
	     d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: (#"-") :: t1 :: t2 :: t3 :: t4 :: [] =>
	       cpr_ok(d1,d2,m1,m2,y1,y2,t1,t2,t3,t4) err_msg
	   | d1 :: d2 :: m1 :: m2 :: y1 :: y2 :: t1 :: t2 :: t3 :: t4 :: [] =>
	       cpr_ok(d1,d2,m1,m2,y1,y2,t1,t2,t3,t4) err_msg
	   | _ => ("", err_msg :: errs)
	 end
     | _ => ("", gen_err_msg(fvar_text,$"Der er mere end et <b>cpr-nummer</b>.?" && add) :: errs))
       handle _ => ("", gen_err_msg(fvar_text,add) :: errs)
  end

fun chk_enum form_var enums fvar_text errs =
  let
    val add = 
      sl2wseq_nl ["Du skal indtaste en af de følgende værdier:",
		  "<blockquote>"] &&
      (prsep ($ ",") $ enums) &&
      ($ "\n</blockquote>\n")
  in
    (case Mosmlcgi.cgi_field_strings form_var of
     [] => ("", gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste formvariablen ",
					       form_var]) :: errs)
   | [fvar] =>
       let
	 val fvar' = trim_str_fb fvar
       in
	 case List.find (fn enum => fvar' = enum) enums
	   of NONE => ("", gen_err_msg(fvar_text,sl2wseq ["Du indtastede '",
							  fvar',
							  "' som ikke findes i listen nedenfor."] && add) :: errs)
	   | SOME _ => (fvar', errs)
       end
   | _ => ("", gen_err_msg(fvar_text,sl2wseq ["Der er mere end en formvariabel '",
					      form_var,
					      "?"]) :: errs))
  end

(* Given a list of strings, checks that they are mutually different. *)
fun chk_enums_are_different enums fn_eq err_text errs =
  let
    fun check(c,ls1,ls2) = not (List.exists (fn_eq c) (ls1@ls2))
    fun check_all ([],ls2) = true
      | check_all (x::xs,ls2) = check(x,xs,ls2) andalso check_all(xs,x::ls2)
  in
    if check_all (enums,[]) then
      errs
    else
      gen_err_msg(err_text, $"Der er ens valg.") :: errs
  end

(* Check that the date follows the DDMMYY syntax, and that its actually a valid date. *)
fun chk_dateDDMMYYYY form_var fvar_text errs =
  (case Mosmlcgi.cgi_field_strings form_var of
     [] => ((0,0,0), gen_err_msg(fvar_text, $"Du skal indtaste en <b>dato</b> i format \"DDMMYY\", f.eks. \"291270\"") :: errs)
   | [dstr] =>
       (case regex2 "^([0-9][0-9])([0-9][0-9])([0-9][0-9][0-9][0-9])$" (trim_str dstr) of
	  NONE => ((0,0,0), gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste en <b>dato</b> i format \"DDMMYYYY\", f.eks. \"29121970\" - '",
							   dstr,
							   "' er ikke en sådan dato"]) :: errs)
	| SOME [all,dd,mm,yyyy] =>
	    let
	      val (dd,mm,yyyy) = (Option.valOf (Int.fromString dd), Option.valOf (Int.fromString mm), Option.valOf (Int.fromString yyyy))
	    in
	      if ItcDate.date_ok dd mm yyyy then
		((dd,mm,yyyy),errs)
	      else
		((0,0,0), gen_err_msg(fvar_text,sl2wseq ["Du har indtastet en dato " ^ dstr ^ " som ikke eksisterer."]) :: errs)
	    end)
   | _ => ((0,0,0), gen_err_msg(fvar_text,sl2wseq ["Der er mere end en formvariabel '",
						   form_var,
						   "?"]) :: errs))
  handle _ => ((0,0,0), gen_err_msg(fvar_text,
				    sl2wseq ["Du skal indtaste en <b>dato</b> i format \"DDMMYY\", f.eks. \"291270\""]) :: errs)

(* Check that the date follows the DD/MM-YYYY syntax, and that its actually a valid date. *)
fun chk_dateDDMMYYYYdk form_var fvar_text errs =
  (case Mosmlcgi.cgi_field_strings form_var of
     [] => ((0,0,0), gen_err_msg(fvar_text,$"Du skal indtaste en <b>dato</b> i format \"DD/MM-YYYY\", f.eks. \"29/12-1970\"") :: errs)
   | [dstr] =>
       (case regex2 "^([0-9][0-9])/([0-9][0-9])-([0-9][0-9][0-9][0-9])$" (trim_str dstr) of
	  NONE => ((0,0,0), gen_err_msg(fvar_text,sl2wseq ["Du skal indtaste en <b>dato</b> i format ",
	                                                   "\"DD/MM-YYYY\", f.eks. \"29/12-1970\" - '",
							   dstr,
							   "' er ikke en sådan dato"]) :: errs)
	| SOME[all,dd,mm,yyyy] =>
	    let
	      val (dd,mm,yyyy) = (Option.valOf (Int.fromString dd), Option.valOf (Int.fromString mm), Option.valOf (Int.fromString yyyy))
	    in
	      if ItcDate.date_ok dd mm yyyy then
		((dd,mm,yyyy),errs)
	      else
		((0,0,0), gen_err_msg(fvar_text,$("Du har indtastet en dato " ^ dstr ^ " som ikke eksisterer.")) :: errs)
	    end)
   | _ => ((0,0,0), gen_err_msg(fvar_text,sl2wseq ["Der er mere end en formvariabel '",
						   form_var,
						   "?"]) :: errs))
  handle _ => ((0,0,0), gen_err_msg(fvar_text,
				    sl2wseq ["Du skal indtaste en <b>dato</b> i format \"DD/MM-YYYY\", f.eks. \"29/12-1970\""]) :: errs)


(* If form_var is non-existing 0 or negative then it is mal formed. *)
fun chk_seq_id return_panic_error form_var =
  case Mosmlcgi.cgi_field_strings form_var of
    [] => return_panic_error(sl2wseq ["Sequence id ", form_var, " is missing"])
  | [fvar] =>
      ((case Int.fromString fvar of
	  NONE => return_panic_error(sl2wseq ["Sequence id ", form_var, " with value '",
					      fvar," is not a sequence number"])
	| SOME uint' => 
	    if uint' > 0 then 
	      uint'
	    else
	      return_panic_error(sl2wseq ["Sequence id ", form_var, " with value '",
					  fvar," should be positive."]))
	  handle Overflow => return_panic_error(sl2wseq ["Sequence id ", form_var, " with value '",
							 fvar," resulted in overflow."]))
  | _ => return_panic_error(sl2wseq ["More than one Sequence id ", form_var, " exists."])

fun chk_enum_panic return_panic_error form_var enums =
  let
    val add = 
      sl2wseq_nl ["Du skal indtaste en af de følgende værdier:",
		  "<blockquote>"] &&
      (prsep ($ ",") $ enums) &&
      ($ "\n</blockquote>\n")
  in
    (case Mosmlcgi.cgi_field_strings form_var of
     [] => return_panic_error(sl2wseq ["Formvariablen ",
				     form_var, " mangler"])
   | [fvar] =>
       let
	 val fvar' = trim_str_fb fvar
       in
	 case List.find (fn enum => fvar' = enum) enums
	   of NONE => return_panic_error(sl2wseq ["Formvariablen '",
						  fvar',
						  "' findes ikke i listen nedenfor."] && add)
	   | SOME _ => fvar'
       end
   | _ => return_panic_error (sl2wseq ["Der er mere end en formvariabel '",
				    form_var,
				    "?"]))
  end

fun chk_name_panic return_panic_error form_var =
  let 
    val add =
      sl2wseq_nl ["<blockquote>",
		  "Et navn må indeholde bogstaver fra alfabetet ",
		  "samt disse tegn: <b>'</b>, <b>\\ </b> og <b>-</b>.",
		  "</blockquote>"]
  in
    case Mosmlcgi.cgi_field_strings form_var of 
      []  => return_panic_error($ "Du skal indtaste et <b>navn</b>." && add)
    | [name] => 
	if regex "^[a-zA-ZA0-9ÆØÅaæøå '\\-]+$" (trim_str_fb name) then
	  trim_str_fb name
	else
	  return_panic_error(sl2wseq ["Du skal indtaste et <b>navn</b> - `",
				      trim_str_fb name,
				      " er ikke et korrekt navn"] && add)
    | _ => return_panic_error($ "Der er mere end et <b>navn</b>?" && add)
  end

end


*)



(* 

  signature FORMVAR =
    sig
      val getNat      : string -> int option
      val getInt      : string -> int option
      val getReal     : string -> real option
      val getNum      : string -> real option
      val getString   : string -> string option
      val getIntRange : int -> int -> string -> int option

      val getNatOrFail    : string -> int
      val getIntOrFail    : string -> int
      val getRealOrFail   : string -> real
      val getNumOrFail    : string -> real
      val getStringOrFail : string -> string
      val getIntRangeOrFail : int -> int -> string -> int

      val returnError : string -> 'a
    end
  
structure FormVar : FORMVAR =
  struct
    fun returnError (err:string) : 'a = 
      (Ns.return
       `<html>
         <head><title>Form Error</title></head>
         <body bgcolor=white>
            <h2>Error processing form data</h2>
            ^err <hr> <i>Served by SMLserver</i>
         </body>
        </html>`;
       Ns.exit())

    fun wrapFail (f : string->'a) (s:string): 'a =
      f s handle Fail err => returnError err

    fun wrapOption (f : string->'a) (s:string): 'a option =
      SOME (f s) handle Fail err => NONE
      
    fun fail s = raise Fail s

    fun noFormVar s       = fail ("No form variable '" ^ s ^ "'") 
    fun emptyFormVar s    = fail ("Empty form variable '" ^ s ^ "'")       
    fun typeMismatch ty s = fail ("Form variable '" ^ s ^ "' should be " ^ ty)
    fun tooLarge ty s     = fail (ty ^ " in form variable '" ^ s ^ "' is too large")

    fun getNat0 (fv : string) : int =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "a positive integer" fv) 
		       handle Overflow => tooLarge "Positive integer" fv)
		 else typeMismatch "a positive integer" fv
	        | nil => emptyFormVar fv
	  end
    fun getInt0 (fv: string) : int =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "an integer" fv) 
		       handle Overflow => tooLarge "Integer" fv)
		 else typeMismatch "an integer" fv
	        | nil => emptyFormVar fv
	  end
    fun getReal0 (fv: string) : real =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "a real" fv) 
		       handle Overflow => tooLarge "Real" fv)
		 else typeMismatch "a real" fv
	        | nil => emptyFormVar fv
	  end
    fun getNum0 (fv: string) : real =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => n
			| _ => 
			 (case Int.scan StringCvt.DEC List.getItem l
			    of SOME (n, nil) => real n
			     | _ => typeMismatch "a numeral" fv)) 
		       handle Overflow => tooLarge "Numeral" fv)
		 else typeMismatch "a numeral" fv
	        | nil => emptyFormVar fv
	  end
    fun getString0 (fv: string) : string = 
      case Ns.Conn.formvar fv of
	SOME s => if size s = 0 then emptyFormVar fv 
		  else s
      | NONE => noFormVar fv

    fun getIntRange0 a b (fv: string) : int =
      let val i = getInt0 fv
      in if a <= i andalso i <= b then i
	 else fail ("Integer form variable `" ^ fv ^ "' is out of range")
      end

    val getNat    = wrapOption getNat0
    val getInt    = wrapOption getInt0
    val getReal   = wrapOption getReal0
    val getNum    = wrapOption getNum0
    val getString = wrapOption getString0
    fun getIntRange a b = wrapOption (getIntRange0 a b)

    val getNatOrFail    = wrapFail getNat0
    val getIntOrFail    = wrapFail getInt0
    val getRealOrFail   = wrapFail getReal0
    val getNumOrFail    = wrapFail getNum0
    val getStringOrFail = wrapFail getString0
    fun getIntRangeOrFail a b = wrapFail (getIntRange0 a b)
  end


*)