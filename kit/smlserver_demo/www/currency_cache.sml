structure FV = FormVar

fun getdate () = 
  Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()))

fun round r = Real.fmt (StringCvt.FIX(SOME 2)) r

val a = FV.wrapFail FV.getRealErr ("a", "amount")
val a_str = Real.toString a
val s = FV.wrapFail FV.getStringErr ("s", "source currency")
val t = FV.wrapFail FV.getStringErr ("t", "target currency")

fun return_page body = Page.return 
  ("Currency Exchange Service, " ^ getdate()) body

val url = 
  ("http://se.finance.yahoo.com/m5?s=" ^ Ns.encodeUrl s 
   ^ "&t=" ^ Ns.encodeUrl t)

fun return_err_page () = return_page 
  `The service is currently not available, probably 
  because we had trouble getting information from the 
  data source: <a href="^url">^url</a>.`

val pattern = RegExp.fromString 
  (".+" ^ s ^ t ^ ".+<td>([0-9]+).([0-9]+)</td>.+")

val fetch = Ns.Cache.cacheForAwhile
  (fn url => 
   case Ns.fetchUrl url of 
     NONE => "" 
   | SOME pg => 
       (case RegExp.extract pattern pg of
	  SOME [rate1, rate2] => rate1^"."^rate2
	| _ => ""), "currency", 5*60)

val _ =
  case fetch url of
    "" => return_err_page()
  | rate_str =>
      let val rate = Option.valOf (Real.fromString rate_str)
      in return_page 
	`^a_str ^s gives ^(round (a*rate)) ^t.<p>
	The exchange rate is obtained by fetching<p>
	<a href="^url">^url</a><p>
	New <a href="currency_cache.html">Calculation</a>`
      end
