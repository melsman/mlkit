fun getdate () = Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()))
fun round r = Real.fmt (StringCvt.FIX(SOME 2)) r

val a = FormVar.wrapFail FormVar.getRealErr ("a", "amount")
val a_str = (Real.toString a)
val s = FormVar.wrapFail FormVar.getStringErr ("s", "source currency")
val t = FormVar.wrapFail FormVar.getStringErr ("t", "target currency")

fun return_page body =
  Ns.return (`<html>
   <head>
   <title>Currency Service</title>
   </head>
   <body bgcolor=white>
  <h2>Currency Service, ^(getdate())</h2><p>`
   ^^ body ^^ `
  <hr>
  <a href="http://www.smlserver.org/">SMLserver Home Page</a> 
  (<a href="mailto:smlserver@it.edu">smlserver@it.edu</a>)
   </body>
   </html>`)

val url = "http://se.finance.yahoo.com/m5?a="
  ^ (Ns.encodeUrl a_str) ^ "&s=" ^ (Ns.encodeUrl s) 
  ^ "&t=" ^ (Ns.encodeUrl t)

fun return_err_page () = return_page 
  `The service is currently not available, because we have trouble 
   getting information from the data source: <a href="^url">^url</a>. <p>
   Please send us an <a href=\"mailto:smlserver@it.edu\">email</a>.`

val pattern = RegExp.fromString (".+" ^ s ^ t ^ ".+<td>([0-9]+).([0-9]+)</td>.+")
val _ =
  case Ns.fetchUrl url
    of NONE => return_err_page()
  | SOME pg =>
     (case RegExp.extract pattern pg
	of SOME [rate1, rate2] =>
	  let 
	    val rate = Option.valOf (Real.fromString (rate1^"."^rate2))
	  in
	    return_page `^a_str (^s) gives you ^((round (a*rate))) (^t).<p>
            The rate used is ^(round rate) and is obtained from <a href="^url">^url</a>.<p>
            New <a href="currency.html">Calculation</a>`
	  end
	| _  => return_err_page())
