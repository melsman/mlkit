  val getReal = FormVar.wrapFail FormVar.getRealErr
  val getString = FormVar.wrapFail FormVar.getStringErr

  val a = getReal ("a", "amount")
  val s = getString ("s", "source currency")
  val t = getString ("t", "target currency")

  fun errPage () = 
    (Page.return "Currency Service Error"
     `The service is currently not available, probably 
      because we have trouble getting information from 
      the data source: <a href="^url">^url</a>.`
      ; Ns.exit())

  val url = "http://se.finance.yahoo.com/m5?s=" ^ 
    Ns.encodeUrl s ^ "&t=" ^ Ns.encodeUrl t

  val pg = case Ns.fetchUrl url 
	     of NONE => errPage()
	      | SOME pg => pg

  val pattern = RegExp.fromString 
    (".+" ^ s ^ t ^ ".+<td>([0-9]+).([0-9]+)</td>.+")

  fun getdate() = 
    Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()))

  fun round r = 
    Real.fmt (StringCvt.FIX(SOME 2)) r

  val _ =
    case RegExp.extract pattern pg 
      of SOME [rate1, rate2] =>
	(let 
	   val rate = Option.valOf 
	     (Real.fromString (rate1^"."^rate2))
	 in
	   Page.return ("Currency Service - " ^ getdate())
	   `^(Real.toString a) (^s) gives you 
	    ^((round (a*rate))) (^t).<p> The rate used 
	    is ^(round rate) and is obtained from 
	    <a href="^url">^url</a>.<p>
	    New <a href="currency.html">Calculation</a>?`
	 end handle _ => errPage())
       | _  => errPage()
