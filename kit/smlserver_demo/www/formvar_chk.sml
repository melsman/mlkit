(* Collect All Errors in one final Error Page *)

val (i,errs) = FormVar.getIntErr("int","integer",FormVar.emptyErr)
val (n,errs) = FormVar.getNatErr("nat","positive integer",errs)
val (r,errs) = FormVar.getRealErr("real","floating point",errs)
val (str,errs) = FormVar.getStringErr("str","string",errs)
val (range,errs) = FormVar.getIntRangeErr 2 10 ("range","range",errs)
val _ = FormVar.anyErrors errs

(* Show only one error at the time *)
(*
val i = (FormVar.wrapFail FormVar.getIntErr) ("int","integer")
val n = (FormVar.wrapFail FormVar.getNatErr) ("nat","positive integer")
*)

(* Raise Exceptions *)
(*
val i = (FormVar.wrapExn FormVar.getIntErr) "int"
val n = (FormVar.wrapExn FormVar.getNatErr) "nat"
*)

(* Return SOME v on success; otherwise NONE *)
(*
val i = Option.valOf(FormVar.wrapOpt FormVar.getIntErr "int")
val n = Option.valOf(FormVar.wrapOpt FormVar.getNatErr "nat")
*)

val _ =
Ns.return (`
<html>
<head>
<title>Result of Checking Form Variables</title>
</head>
<body bgcolor=white>
<h2>Result of Checking Form Variables</h2><p>
You provided the following information:<p>

The integer: ^(Int.toString i)<p>
The positive integer: ^(Int.toString n)<p>
The real: ^(Real.toString r)<p>
The string: ^str<p>
The range value: ^(Int.toString range)<p>

<hr>
<a href="http://www.smlserver.org/">SMLserver Home Page</a> 
(<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-20
</body>
</html>`)
