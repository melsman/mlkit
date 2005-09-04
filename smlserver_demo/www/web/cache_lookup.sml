val kind = Option.valOf (Web.Conn.formvar "kind") handle _ => "Size"

val cache = 
  let
    val (k,name) =
      case kind of
         "WhileUsed" => (Web.Cache.WhileUsed (SOME(Time.fromSeconds 20),SOME(10000)),"users1")
       | "TimeOut" => (Web.Cache.TimeOut (SOME(Time.fromSeconds 20), SOME(10000)),"users2")
       | "Size" => (Web.Cache.WhileUsed (NONE, SOME(10000)),"users3")
  in
    Web.Cache.get (Web.Cache.String, 
		  Web.Cache.Pair Web.Cache.Int Web.Cache.String,
		  name, k)
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
	   of SOME(uid,name) => "Name and userid for " ^ email ^ " is: (" ^ name ^ "," ^ (Int.toString uid) ^ ")"
	 | NONE => "No name in cache for " ^ email)
	   


