(* Collect All Errors in one final Error Page *)

structure FV = ScsFormVar

val (i,errs)     = FV.getIntErr("int","integer",FV.emptyErr)
val (n,errs)     = FV.getNatErr("nat","positive integer",errs)
val (r,errs)     = FV.getRealErr("real","floating point",errs)
val (str,errs)   = FV.getStringErr("str","string",errs)
val (range,errs) = FV.getIntRangeErr 2 10 ("range","range",errs)
val (email,errs) = FV.getEmailErr ("email","an IT-C email",errs)
val (name,errs)  = FV.getNameErr ("name","first name",errs)
val (login,errs) = FV.getLoginErr ("login","personal IT-C login",errs)
val (phone,errs) = FV.getPhoneErr ("phone","Work Phone",errs)
val (url,errs)   = FV.getUrlErr ("url", "URL of your private homepage",errs)
val (cpr,errs)   = FV.getCprErr ("cpr", "your cpr number", errs)
val (sex,errs)   = FV.getEnumErr ["Female","Male","Unknown"] ("sex", "your sex", errs)
val (date,errs)  = FV.getDateIso ("date","birthdate",errs)
val _ = FV.anyErrors errs


(* Show only one error at the time *)
(*
val i     = (FV.wrapFail FV.getIntErr) ("int","integer")
val n     = (FV.wrapFail FV.getNatErr) ("nat","positive integer")
val r     = (FV.wrapFail FV.getRealErr) ("real","floating point")
val str   = (FV.wrapFail FV.getStringErr) ("str","string")
val range = (FV.wrapFail (FV.getIntRangeErr 2 10)) ("range","range")
val email = (FV.wrapFail FV.getEmailErr) ("email","an IT-C email")
val name  = (FV.wrapFail FV.getNameErr) ("name","first name")
val login = (FV.wrapFail FV.getLoginErr) ("login","personal IT-C login")
val phone = (FV.wrapFail FV.getPhoneErr) ("phone","Work Phone")
val url   = (FV.wrapFail FV.getUrlErr) ("url", "URL of your private homepage")
val cpr   = (FV.wrapFail FV.getCprErr) ("cpr", "your cpr number")
val sex   = (FV.wrapFail (FV.getEnumErr ["Female","Male","Unknown"])) ("sex", "your sex")
val date  = (FV.wrapFail FV.getDateIso) ("date","birthdate")
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
val cpr   = FV.wrapExn FV.getCprErr "cpr"
val sex   = FV.wrapExn (FV.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = FV.wrapExn FV.getDateIso "date"
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
val cpr   = Option.valOf(FV.wrapOpt FV.getCprErr "cpr")
val sex   = Option.valOf(FV.wrapOpt (FV.getEnumErr ["Female","Male","Unknown"]) "sex")
val date  = Option.valOf(FV.wrapOpt FV.getDateIso "date")
*)

(* The Panic wrapper *)
(*val i     = FV.wrapPanic ScsError.panic FV.getIntErr "int"
val n     = FV.wrapPanic ScsError.panic FV.getNatErr "nat"
val r     = FV.wrapPanic ScsError.panic FV.getRealErr "real"
val str   = FV.wrapPanic ScsError.panic FV.getStringErr "str"
val range = FV.wrapPanic ScsError.panic (FV.getIntRangeErr 2 10) "range"
val email = FV.wrapPanic ScsError.panic FV.getEmailErr "email"
val name  = FV.wrapPanic ScsError.panic FV.getNameErr "name"
val login = FV.wrapPanic ScsError.panic FV.getLoginErr "login"
val phone = FV.wrapPanic ScsError.panic FV.getPhoneErr "phone"
val url   = FV.wrapPanic ScsError.panic FV.getUrlErr "url"
val cpr   = FV.wrapPanic ScsError.panic FV.getCprErr "cpr"
val sex   = FV.wrapPanic ScsError.panic (FV.getEnumErr ["Female","Male","Unknown"]) "sex"
val date  = FV.wrapPanic ScsError.panic FV.getDateIso "date"
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
