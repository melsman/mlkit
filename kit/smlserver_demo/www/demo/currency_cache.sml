  val getReal = FormVar.wrapFail FormVar.getRealErr
  val getString = FormVar.wrapFail FormVar.getStringErr

  val a = getReal ("a", "amount")
  val s = getString ("s", "source currency")
  val t = getString ("t", "target currency")

  val url = "http://se.finance.yahoo.com/m5?s=" ^ 
    Ns.encodeUrl s ^ "&t=" ^ Ns.encodeUrl t

  fun errPage () = 
    (Page.return "Currency Service Error"
     `The service is currently not available, probably 
      because we have trouble getting information from 
      the data source: <a href="^url">^url</a>.`
      ; Ns.exit())

  fun getdate () = 
    Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()))

  fun round r = Real.fmt (StringCvt.FIX(SOME 2)) r

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
      "" => errPage ()
    | rate_str =>
	let val rate = Option.valOf (Real.fromString rate_str)
	in Page.return 
	  ("Currency Exchange Service, " ^ getdate()) 
	  `^(Real.toString a) ^s gives ^(round (a*rate)) ^t.<p>
	  The exchange rate is obtained by fetching<p>
	  <a href="^url">^url</a><p>
	  New <a href="currency_cache.html">Calculation</a>`
	end