  structure C = Web.Cache

  val getReal = FormVar.wrapFail FormVar.getRealErr
  val getString = FormVar.wrapFail FormVar.getStringErr

  val a = getReal ("a", "amount")
  val s = getString ("s", "source currency")
  val t = getString ("t", "target currency")

(*  val url = "http://uk.finance.yahoo.com/m5?s=" ^ 
    Web.encodeUrl s ^ "&t=" ^ Web.encodeUrl t *)

  val url = "http://uk.finance.yahoo.com/q?s=" ^
    Web.encodeUrl s ^ Web.encodeUrl t ^ "=X"

  fun errPage () = 
    (Page.return "Currency Service Error"
     `The service is currently not available, probably 
      because we have trouble getting information from 
      the data source: <a href="^url">^url</a>.`
      ; Web.exit())

  fun getdate () = 
    Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()))

  fun round r = Real.fmt (StringCvt.FIX(SOME 2)) r

(*  val pattern = RegExp.fromString 
    (".+" ^ s ^ t ^ ".+<td>([0-9]+).([0-9]+)</td>.+") *)

  val pattern = RegExp.fromString 
    (".+Last Trade:" ^  ".+([0-9]+)\\.([0-9]+).+")

  val cache = C.get (C.String,C.Option C.Real,"currency",
                 C.TimeOut (SOME(Time.fromSeconds(5*60)), SOME(10000)))

  val fetch = C.memoize cache 
    (fn url => case Web.fetchUrl url 
                 of NONE => NONE
                  | SOME pg => 
                   (case RegExp.extract pattern pg 
                      of SOME [r1,r2] => Real.fromString (r1 ^ "." ^ r2)
                       | _ => NONE))

  val _ =
    case fetch url of
      NONE => errPage ()
    | SOME rate =>
	Page.return 
	  ("Currency Exchange Service, " ^ getdate()) 
	  `^(Real.toString a) ^s gives ^(round (a*rate)) ^t.<p>
	  The exchange rate is obtained by fetching<p>
	  <a href="^url">^url</a><p>
	  New <a href="currency_cache.html">Calculation</a>`
