structure C = Cache

val form = 
  `<form method=post action=exchange.sml>
     <b>Dollar amount</b><br><input type=text name=a>
     <input type=submit value="Value in Danish Kroner">
   </form>`
   
val c = C.get ("currency",C.TimeOut 300,C.String,C.Option C.Real)

fun fetchRate url =
  case Ns.fetchUrl url of 
    NONE => NONE
  | SOME pg =>
  let val pattern = RegExp.fromString 
        ".+USDDKK.+<td>([0-9]+).([0-9]+)</td>.+"
  in case RegExp.extract pattern pg 
	 of SOME [r1,r2] => Real.fromString (r1^"."^r2)
	  | _ => NONE
  end

val fetch = C.memoize c fetchRate

val url = "http://se.finance.yahoo.com/m5?s=USD&t=DKK"

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
