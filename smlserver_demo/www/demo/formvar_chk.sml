(* Collect All Errors in one final Error Page *)

structure FV = FormVar

val (i,errs)     = FV.getIntErr("int","integer",FV.emptyErr)
val (n,errs)     = FV.getNatErr("nat","positive integer",errs)
val (r,errs)     = FV.getRealErr("real","floating point",errs)
val (str,errs)   = FV.getStringErr("str","string",errs)
val (range,errs) = FV.getIntRangeErr 2 10 ("range","range",errs)
val (email,errs) = FV.getEmailErr ("email","an email",errs)
val (name,errs)  = FV.getNameErr ("name","first name",errs)
val (login,errs) = FV.getLoginErr ("login","personal login",errs)
val (phone,errs) = FV.getPhoneErr ("phone","Work Phone",errs)
val (url,errs)   = FV.getUrlErr ("url", "URL of your private homepage",errs)
val (sex,errs)   = FV.getEnumErr ["Female","Male","Unknown"] ("sex", "your sex", errs)
val _ = FV.anyErrors errs


(* Show only one error at the time *)
(*
val i     = (FV.wrapFail FV.getIntErr) ("int","integer")
val n     = (FV.wrapFail FV.getNatErr) ("nat","positive integer")
val r     = (FV.wrapFail FV.getRealErr) ("real","floating point")
val str   = (FV.wrapFail FV.getStringErr) ("str","string")
val range = (FV.wrapFail (FV.getIntRangeErr 2 10)) ("range","range")
val email = (FV.wrapFail FV.getEmailErr) ("email","an email")
val name  = (FV.wrapFail FV.getNameErr) ("name","first name")
val login = (FV.wrapFail FV.getLoginErr) ("login","personal login")
val phone = (FV.wrapFail FV.getPhoneErr) ("phone","Work Phone")
val url   = (FV.wrapFail FV.getUrlErr) ("url", "URL of your private homepage")
val sex   = (FV.wrapFail (FV.getEnumErr ["Female","Male","Unknown"])) ("sex", "your sex")
*)

(* Raise Exceptions *)
(*
val i     = FV.wrapExn FV.getIntErr "int"
val n     = FV.wrapExn FV.getNatErr "nat"
val r     = FV.wrapExn FV.getRealErr "real"
val str   = FV.wrapExn FV.getStringErr "str"
val range = FV.wrapExn (FV.getIntRangeErr 2 10) "range"
val email = FV.wrapExn FV.getEmailErr "email"
val name  = FV.wrapExn FV.getNameErr "name"
val login = FV.wrapExn FV.getLoginErr "login"
val phone = FV.wrapExn FV.getPhoneErr "phone"
val url   = FV.wrapExn FV.getUrlErr "url"
val sex   = FV.wrapExn (FV.getEnumErr ["Female","Male","Unknown"]) "sex"
*)

(* Return SOME v on success; otherwise NONE *)
(*
val i     = Option.valOf(FV.wrapOpt FV.getIntErr "int")
val n     = Option.valOf(FV.wrapOpt FV.getNatErr "nat")
val r     = Option.valOf(FV.wrapOpt FV.getRealErr "real")
val str   = Option.valOf(FV.wrapOpt FV.getStringErr "str")
val range = Option.valOf(FV.wrapOpt (FV.getIntRangeErr 2 10) "range")
val email = Option.valOf(FV.wrapOpt FV.getEmailErr "email")
val name  = Option.valOf(FV.wrapOpt FV.getNameErr "name")
val login = Option.valOf(FV.wrapOpt FV.getLoginErr "login")
val phone = Option.valOf(FV.wrapOpt FV.getPhoneErr "phone")
val url   = Option.valOf(FV.wrapOpt FV.getUrlErr "url")
val sex   = Option.valOf(FV.wrapOpt (FV.getEnumErr ["Female","Male","Unknown"]) "sex")
*)

(* The Panic wrapper *)
(*
val i     = FV.wrapPanic Page.panic FV.getIntErr "int"
val n     = FV.wrapPanic Page.panic FV.getNatErr "nat"
val r     = FV.wrapPanic Page.panic FV.getRealErr "real"
val str   = FV.wrapPanic Page.panic FV.getStringErr "str"
val range = FV.wrapPanic Page.panic (FV.getIntRangeErr 2 10) "range"
val email = FV.wrapPanic Page.panic FV.getEmailErr "email"
val name  = FV.wrapPanic Page.panic FV.getNameErr "name"
val login = FV.wrapPanic Page.panic FV.getLoginErr "login"
val phone = FV.wrapPanic Page.panic FV.getPhoneErr "phone"
val url   = FV.wrapPanic Page.panic FV.getUrlErr "url"
val sex   = FV.wrapPanic Page.panic (FV.getEnumErr ["Female","Male","Unknown"]) "sex"
*)

val _ = Page.return "Result of Checking Form Variables" `
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
The Sex is: ^sex<p>`
