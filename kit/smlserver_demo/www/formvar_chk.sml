(* Collect All Errors in one final Error Page *)

val (i,errs)     = ScsFormVar.getIntErr("int","integer",ScsFormVar.emptyErr)
val (n,errs)     = ScsFormVar.getNatErr("nat","positive integer",errs)
val (r,errs)     = ScsFormVar.getRealErr("real","floating point",errs)
val (str,errs)   = ScsFormVar.getStringErr("str","string",errs)
val (range,errs) = ScsFormVar.getIntRangeErr 2 10 ("range","range",errs)
val (email,errs) = ScsFormVar.getEmailErr ("email","an IT-C email",errs)
val (name,errs)  = ScsFormVar.getNameErr ("name","first name",errs)
val (login,errs) = ScsFormVar.getLoginErr ("login","personal IT-C login",errs)
val (phone,errs) = ScsFormVar.getPhoneErr ("phone","Work Phone",errs)
val (url,errs)   = ScsFormVar.getUrlErr ("url", "URL of your private homepage",errs)
val (cpr,errs)   = ScsFormVar.getCprErr ("cpr", "your cpr number", errs)
val (sex,errs)   = ScsFormVar.getEnumErr ["Female","Male","Unknown"] ("sex", "your sex", errs)
val (date,errs)  = ScsFormVar.getDateIso ("date","birthdate",errs)
val _ = ScsFormVar.anyErrors errs


(* Show only one error at the time *)
(*
val i     = (ScsFormVar.wrapFail ScsFormVar.getIntErr) ("int","integer")
val n     = (ScsFormVar.wrapFail ScsFormVar.getNatErr) ("nat","positive integer")
val r     = (ScsFormVar.wrapFail ScsFormVar.getRealErr) ("real","floating point")
val str   = (ScsFormVar.wrapFail ScsFormVar.getStringErr) ("str","string")
val range = (ScsFormVar.wrapFail (ScsFormVar.getIntRangeErr 2 10)) ("range","range")
val email = (ScsFormVar.wrapFail ScsFormVar.getEmailErr) ("email","an IT-C email")
val name  = (ScsFormVar.wrapFail ScsFormVar.getNameErr) ("name","first name")
val login = (ScsFormVar.wrapFail ScsFormVar.getLoginErr) ("login","personal IT-C login")
val phone = (ScsFormVar.wrapFail ScsFormVar.getPhoneErr) ("phone","Work Phone")
val url   = (ScsFormVar.wrapFail ScsFormVar.getUrlErr) ("url", "URL of your private homepage")
val cpr   = (ScsFormVar.wrapFail ScsFormVar.getCprErr) ("cpr", "your cpr number")
val sex   = (ScsFormVar.wrapFail (ScsFormVar.getEnumErr ["Female","Male","Unknown"])) ("sex", "your sex")
val date  = (ScsFormVar.wrapFail ScsFormVar.getDateIso) ("date","birthdate")
*)

(* Raise Exceptions *)
(*
val i     = ScsFormVar.wrapExn ScsFormVar.getIntErr "int"
val n     = ScsFormVar.wrapExn ScsFormVar.getNatErr "nat"
val r     = ScsFormVar.wrapExn ScsFormVar.getRealErr "real"
val str   = ScsFormVar.wrapExn ScsFormVar.getStringErr "str"
val range = ScsFormVar.wrapExn (ScsFormVar.getIntRangeErr 2 10) "range"
val email = ScsFormVar.wrapExn ScsFormVar.getEmailErr "email"
val name  = ScsFormVar.wrapExn ScsFormVar.getNameErr "name"
val login = ScsFormVar.wrapExn ScsFormVar.getLoginErr "login"
val phone = ScsFormVar.wrapExn ScsFormVar.getPhoneErr "phone"
val url   = ScsFormVar.wrapExn ScsFormVar.getUrlErr "url"
val cpr   = ScsFormVar.wrapExn ScsFormVar.getCprErr "cpr"
val sex   = ScsFormVar.wrapExn (ScsFormVar.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = ScsFormVar.wrapExn ScsFormVar.getDateIso "date"
*)

(* Return SOME v on success; otherwise NONE *)
(*
val i     = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getIntErr "int")
val n     = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getNatErr "nat")
val r     = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getRealErr "real")
val str   = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getStringErr "str")
val range = Option.valOf(ScsFormVar.wrapOpt (ScsFormVar.getIntRangeErr 2 10) "range")
val email = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getEmailErr "email")
val name  = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getNameErr "name")
val login = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getLoginErr "login")
val phone = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getPhoneErr "phone")
val url   = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getUrlErr "url")
val cpr   = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getCprErr "cpr")
val sex   = Option.valOf(ScsFormVar.wrapOpt (ScsFormVar.getEnumErr ["Female","Male","Unknown"]) "sex")
val date  = Option.valOf(ScsFormVar.wrapOpt ScsFormVar.getDateIso "date")
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
val i     = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getIntErr "int"
val n     = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getNatErr "nat"
val r     = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getRealErr "real"
val str   = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getStringErr "str"
val range = ScsFormVar.wrapPanic genPanicPg (ScsFormVar.getIntRangeErr 2 10) "range"
val email = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getEmailErr "email"
val name  = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getNameErr "name"
val login = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getLoginErr "login"
val phone = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getPhoneErr "phone"
val url   = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getUrlErr "url"
val cpr   = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getCprErr "cpr"
val sex   = ScsFormVar.wrapPanic genPanicPg (ScsFormVar.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = ScsFormVar.wrapPanic genPanicPg ScsFormVar.getDateIso "date"
*)

val _ = ScsPage.returnPg "Result of Checking Form Variables" `
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
The Date is: ^date<p>`
