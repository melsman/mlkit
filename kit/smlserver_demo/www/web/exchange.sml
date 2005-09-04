structure C = Web.Cache

val form = 
  `<form method=post action=exchange.sml>
     <b>Dollar amount</b><br><input type=text name=a>
     <input type=submit value="Value in Danish Kroner">
   </form>`
   
val cache = C.get (C.String,C.Option C.Real,"currency",
              C.TimeOut (SOME(Time.fromSeconds 300), SOME(10000)))

fun fetchRate url =
  case Web.fetchUrl url of 
    NONE => NONE
  | SOME pg =>
  let val pattern = RegExp.fromString 
        ".+USDDKK.+<td>([0-9]+).([0-9]+)</td>.+"
  in case RegExp.extract pattern pg 
	 of SOME [r1,r2] => Real.fromString (r1^"."^r2)
	  | _ => NONE
  end

val fetch = C.memoize cache fetchRate

val url = "http://uk.finance.yahoo.com/m5?s=USD&t=DKK"

val body =
  case FormVar.wrapOpt FormVar.getRealErr "a" of
    NONE => form
  | SOME a =>
  case fetch url of
   NONE  => `The service is currently not available`
  | SOME rate =>
     `^(Real.toString a) USD gives 
      ^(Real.fmt (StringCvt.FIX(SOME 2)) (a*rate)) DKK.
      <p>` ^^ form

val _ = Page.return "Currency Exchange Service" body
