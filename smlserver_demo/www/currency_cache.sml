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
  (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-08-08
   </body>
   </html>`)

val url = "http://se.finance.yahoo.com/m5?a="^(Ns.encodeUrl a_str)^"&s="^(Ns.encodeUrl s)^"&t="^(Ns.encodeUrl t)
fun return_err_page () =
  return_page `The service is currently not available, because we have trouble 
               getting information from the data source: <a href="^url">^url</a>. <p>
               Please send me an <a href=\"mailto:nh@itu.dk\">email</a>.`

val pattern = s^t^".+<td>([0-9]+).([0-9]+)</td>"
val fetch = Ns.Cache.cacheForAwhile
  (fn url => 
   case Ns.fetchUrl url of 
     NONE => "" 
   | SOME pg => 
       (case RegExp.regExp pattern pg of
	  SOME [_, rate1, rate2] => rate1^"."^rate2
	| NONE => ""), "currency", 300)

val _ =
  case fetch url of
    "" => return_err_page()
  | rate_str =>
      let 
	val rate = Option.valOf (Real.fromString rate_str)
      in
	return_page `^a_str (^s) gives you ^((round (a*rate))) (^t).<p>
	The rate used is ^(round rate) and is obtained from <a href="^url">^url</a>.<p>
	New <a href="currency.html">Calculation</a>`
      end
