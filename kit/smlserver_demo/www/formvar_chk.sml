(* Collect All Errors in one final Error Page *)

val (i,errs)     = FormVar.getIntErr("int","integer",FormVar.emptyErr)
val (n,errs)     = FormVar.getNatErr("nat","positive integer",errs)
val (r,errs)     = FormVar.getRealErr("real","floating point",errs)
val (str,errs)   = FormVar.getStringErr("str","string",errs)
val (range,errs) = FormVar.getIntRangeErr 2 10 ("range","range",errs)
val (email,errs) = FormVar.getEmailErr ("email","an IT-C email",errs)
val (name,errs)  = FormVar.getNameErr ("name","first name",errs)
val (login,errs) = FormVar.getLoginErr ("login","personal IT-C login",errs)
val (phone,errs) = FormVar.getPhoneErr ("phone","Work Phone",errs)
val (url,errs)   = FormVar.getUrlErr ("url", "URL of your private homepage",errs)
val (cpr,errs)   = FormVar.getCprErr ("cpr", "your cpr number", errs)
val (sex,errs)   = FormVar.getEnumErr ["Female","Male","Unknown"] ("sex", "your sex", errs)
val (date,errs)  = FormVar.getDateIso ("date","birthdate",errs)
val _ = FormVar.anyErrors errs


(* Show only one error at the time *)
(*
val i     = (FormVar.wrapFail FormVar.getIntErr) ("int","integer")
val n     = (FormVar.wrapFail FormVar.getNatErr) ("nat","positive integer")
val r     = (FormVar.wrapFail FormVar.getRealErr) ("real","floating point")
val str   = (FormVar.wrapFail FormVar.getStringErr) ("str","string")
val range = (FormVar.wrapFail (FormVar.getIntRangeErr 2 10)) ("range","range")
val email = (FormVar.wrapFail FormVar.getEmailErr) ("email","an IT-C email")
val name  = (FormVar.wrapFail FormVar.getNameErr) ("name","first name")
val login = (FormVar.wrapFail FormVar.getLoginErr) ("login","personal IT-C login")
val phone = (FormVar.wrapFail FormVar.getPhoneErr) ("phone","Work Phone")
val url   = (FormVar.wrapFail FormVar.getUrlErr) ("url", "URL of your private homepage")
val cpr   = (FormVar.wrapFail FormVar.getCprErr) ("cpr", "your cpr number")
val sex   = (FormVar.wrapFail (FormVar.getEnumErr ["Female","Male","Unknown"])) ("sex", "your sex")
val date  = (FormVar.wrapFail FormVar.getDateIso) ("date","birthdate")
*)

(* Raise Exceptions *)
(*
val i     = FormVar.wrapExn FormVar.getIntErr "int"
val n     = FormVar.wrapExn FormVar.getNatErr "nat"
val r     = FormVar.wrapExn FormVar.getRealErr "real"
val str   = FormVar.wrapExn FormVar.getStringErr "str"
val range = FormVar.wrapExn (FormVar.getIntRangeErr 2 10) "range"
val email = FormVar.wrapExn FormVar.getEmailErr "email"
val name  = FormVar.wrapExn FormVar.getNameErr "name"
val login = FormVar.wrapExn FormVar.getLoginErr "login"
val phone = FormVar.wrapExn FormVar.getPhoneErr "phone"
val url   = FormVar.wrapExn FormVar.getUrlErr "url"
val cpr   = FormVar.wrapExn FormVar.getCprErr "cpr"
val sex   = FormVar.wrapExn (FormVar.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = FormVar.wrapExn FormVar.getDateIso "date"
*)

(* Return SOME v on success; otherwise NONE *)
(*
val i     = Option.valOf(FormVar.wrapOpt FormVar.getIntErr "int")
val n     = Option.valOf(FormVar.wrapOpt FormVar.getNatErr "nat")
val r     = Option.valOf(FormVar.wrapOpt FormVar.getRealErr "real")
val str   = Option.valOf(FormVar.wrapOpt FormVar.getStringErr "str")
val range = Option.valOf(FormVar.wrapOpt (FormVar.getIntRangeErr 2 10) "range")
val email = Option.valOf(FormVar.wrapOpt FormVar.getEmailErr "email")
val name  = Option.valOf(FormVar.wrapOpt FormVar.getNameErr "name")
val login = Option.valOf(FormVar.wrapOpt FormVar.getLoginErr "login")
val phone = Option.valOf(FormVar.wrapOpt FormVar.getPhoneErr "phone")
val url   = Option.valOf(FormVar.wrapOpt FormVar.getUrlErr "url")
val cpr   = Option.valOf(FormVar.wrapOpt FormVar.getCprErr "cpr")
val sex   = Option.valOf(FormVar.wrapOpt (FormVar.getEnumErr ["Female","Male","Unknown"]) "sex")
val date  = Option.valOf(FormVar.wrapOpt FormVar.getDateIso "date")
*)

(* The Panic wrapper *)
(*
fun genPanicPg (fv:string,emsg:string) =
  (* This is a phony panic-function. You
     should mail the system administrator
     or simply log the error in the database *)
  (Ns.return `
   <html>
   <head><title>Panic Error</title></head>
   <body bgcolor=white>
   <h2>System Failure.</h2>

   The form variable <b>^fv</b> raised the following system
   failure 
   <blockquote>
   ^emsg
   </blockquote>

   The system administrator has beed notified and 
   the error is logged.<p>
   <hr> <i>Served by SMLserver</i>
   </body>
   </html>`;
   Ns.exit())
val i     = FormVar.wrapPanic genPanicPg FormVar.getIntErr "int"
val n     = FormVar.wrapPanic genPanicPg FormVar.getNatErr "nat"
val r     = FormVar.wrapPanic genPanicPg FormVar.getRealErr "real"
val str   = FormVar.wrapPanic genPanicPg FormVar.getStringErr "str"
val range = FormVar.wrapPanic genPanicPg (FormVar.getIntRangeErr 2 10) "range"
val email = FormVar.wrapPanic genPanicPg FormVar.getEmailErr "email"
val name  = FormVar.wrapPanic genPanicPg FormVar.getNameErr "name"
val login = FormVar.wrapPanic genPanicPg FormVar.getLoginErr "login"
val phone = FormVar.wrapPanic genPanicPg FormVar.getPhoneErr "phone"
val url   = FormVar.wrapPanic genPanicPg FormVar.getUrlErr "url"
val cpr   = FormVar.wrapPanic genPanicPg FormVar.getCprErr "cpr"
val sex   = FormVar.wrapPanic genPanicPg (FormVar.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = FormVar.wrapPanic genPanicPg FormVar.getDateIso "date"
*)

val _ =
Ns.return (`
<html>
<head>
<title>Result of Checking Form Variables</title>
</head>
<body bgcolor=white>
<h2>Result of Checking Form Variables</h2><p>
You provided the following information:<p>

The integer: ^(Int.toString i)<p>
The positive integer: ^(Int.toString n)<p>
The real: ^(Real.toString r)<p>
The string: ^str<p>
The range value: ^(Int.toString range)<p>
The email is: ^email<p>
The name is: ^name<p>
The login is: ^login<p>
The phone number is: ^phone<p>
The URL is: ^url<p>
The Cpr is: ^cpr<p>
The Sex is: ^sex<p>
The Date is: ^date<p>

<hr>
<a href="http://www.smlserver.org/">SMLserver Home Page</a> 
(<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-20
</body>
</html>`)
