structure FV = FormVar

val encdata = FV.wrapOpt FV.getStringErr "encdata"
val decdata = FV.wrapOpt FV.getStringErr "decdata"


val se = case encdata of NONE => "foo"
                   | SOME(a) => a
val te = case encdata of NONE => ``
                   | SOME d => Quot.fromString (Web.encodeUrl d )

(*
val sd = "a"
				  *)
val sd = case decdata of NONE => "foo"
                  | SOME(a) => a
val td = case decdata of NONE => ``
                   | SOME d => Quot.fromString (Web.decodeUrl d )

val _ = Page.return "Encode data" (
`<form method=post action=encode.sml>
<input type=text value=^se name=encdata>
<input type=text value=^sd name=decdata>
<input type=submit value="action">
  </form>` ^^ te ^^ `<br />` ^^ td)
