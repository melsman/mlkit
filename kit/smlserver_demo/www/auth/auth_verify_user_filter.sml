(* auth_verify_user; return user_id if happy, 0 otherwise *)
fun auth_verify_user () =
  let
    val auth_user_id = Ns.Cookie.getCookieValue "auth_user_id"
    val auth_password = Ns.Cookie.getCookieValue "auth_password"
  in
    case (auth_user_id,auth_password) of
      (SOME user_id, SOME psw) =>
	(case Db.zeroOrOneField (Ns.Quot.flatten `select password from auth_user where user_id = '^(Db.qq user_id)'`) of
	   NONE => 0
	 | SOME db_psw => 
	     if db_psw = psw then 
	       (case Int.fromString user_id of
		  NONE => 0
		| SOME u_id => u_id )
	     else 0)
    | _ => 0
  end
handle Ns.Cookie.CookieError _ => 0

(* auth_verify_user_filter; procedure to filter if a
   user is authenticated to request a page *)

fun auth_verify_user_filter () =
  let
    val user_id = auth_verify_user ()
  in
    if user_id = 0 then
      (Ns.returnRedirect "http://localhost:8005/auth/auth_form.sml"; Ns.exit())
    else 
      ()
  end

(* we tell SMLserver to run our cookie checker procedure before
   serving any request for a URL that starts with "/auth/" *)
val _ = Ns.log (Ns.Notice, Ns.Conn.location()^Ns.Conn.url())
val _ = 
  if RegExp.regExpBool "http://nh.itu.dk:8005/auth/admin/.*" (Ns.Conn.location()^Ns.Conn.url()) then
    auth_verify_user_filter ()
  else ()

