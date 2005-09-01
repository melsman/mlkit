val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	       "WhileUsed" => Web.Cache.WhileUsed (SOME(Time.fromSeconds 20), SOME(10000))
       | "TimeOut" => Web.Cache.TimeOut (SOME(Time.fromSeconds 20), SOME(10000))
       | "Size" => Web.Cache.WhileUsed (NONE, SOME(10000))
  in
    Web.Cache.get (Web.Cache.String, 
		  Web.Cache.Triple Web.Cache.String Web.Cache.String Web.Cache.Int,
		  "triple",
		  k)
  end

fun pp_kind kind =
  case kind of
    "Size" => `<b>^kind</b> of size 10000`
  | _ => `<b>^kind</b>. Entries live in the cache in
    approximately 20 seconds.<p>`

fun returnPage s = Page.return "Caching Demonstration"  
  (`^s <p>

  Using cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)

val _ = (* new_p is true if new value added *)
  case Web.Conn.formvar "email"
      of NONE => Web.returnRedirect "cache.sml"
       | SOME email => 
	returnPage
	(case Web.Cache.lookup cache email
	   of SOME (lastname,firstnames,uid) => "Name for " ^ email ^ 
	     " is: (" ^ firstnames ^ "," ^ lastname ^ "," ^ (Int.toString uid) ^ ")"
	 | NONE => "No name in cache for " ^ email)
	   


