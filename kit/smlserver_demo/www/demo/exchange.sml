  val form = 
    `<form method=post action=exchange.sml>
       <b>Dollar amount</b><br><input type=text name=a>
       <input type=submit value="Value in Danish Kroner">
     </form>`
     
  fun fetchRate url =
    case Ns.fetchUrl url of 
      NONE => "" 
    | SOME pg =>
    let val pattern = RegExp.fromString 
          ".+USDDKK.+<td>([0-9]+).([0-9]+)</td>.+"
    in case RegExp.extract pattern pg 
	 of SOME [r1,r2] => r1^"."^r2
	  | _ => ""
    end

  val fetch = Ns.Cache.cacheForAwhile 
    (fetchRate, "currency", 5*60)

  val url = "http://se.finance.yahoo.com/m5?s=USD&t=DKK"

  val body =
    case FormVar.wrapOpt FormVar.getRealErr "a" of
      NONE => form
    | SOME a =>
    case fetch url of
      "" => `The service is currently not available`
    | str =>
    let val rate = Option.valOf (Real.fromString str)
    in `^(Real.toString a) USD gives 
        ^(Real.fmt (StringCvt.FIX(SOME 2)) (a*rate)) DKK.
        <p>` ^^ form
    end

  val _ = Page.return "Currency Exchange Service" body
