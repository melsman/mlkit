
fun get s =
  case Ns.Conn.getQuery(Ns.getConn())
    of SOME formvars => Ns.Set.get(formvars, s)
     | NONE => NONE

fun unique s = 
  case Ns.Conn.getQuery(Ns.getConn())
    of SOME formvars => Ns.Set.unique (formvars, s)
     | NONE => false

fun getFormVar n =
  let 
    fun return_formvar_error n msg =
      (Ns.return("<html><body bgcolor=white><h2>Formvar Error: form variable `" ^ n ^ 
		 "' " ^ msg ^ "</h2>Go back and fill in the form...</body></html>");
       Process.exit Process.success)
  in
    case get n
      of NONE => return_formvar_error n "not available"
       | SOME name => if unique n then name
		      else return_formvar_error n "appears more than once"
  end

val name = getFormVar "name"

val _ =
  Ns.return("<html><body bgcolor=green><h2>Hi " ^ name ^ 
	    "</h2> how are you today?</body></html>")