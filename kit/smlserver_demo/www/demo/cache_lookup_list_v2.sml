val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	"WhileUsed" => NsCacheV2.WhileUsed 20
       | "TimeOut" => NsCacheV2.TimeOut 20
       | "Size" => NsCacheV2.Size 100
  in
    NsCacheV2.get ("userlist",
		   k,
		   NsCacheV2.String, 
		   NsCacheV2.List NsCacheV2.String)
  end

fun pp_kind kind =
  case kind of
    "Size" => `<b>^kind</b> of size 100`
  | _ => `<b>^kind</b>. Entries live in the cache in
    approximately 20 seconds.<p>`

fun returnPage s = Page.return "Caching Demonstration V2"  
  (`^s <p>

  Using cache kind: ` ^^ (pp_kind kind) ^^ `<p>
 
  Go back to <a href=cache_v2.sml?kind=^kind>Cache Demo Home Page</a>.`)

val _ = (* new_p is true if new value added *)
  case Ns.Conn.formvar "email"
      of NONE => Ns.returnRedirect "cache_v2.sml"
       | SOME email => 
	returnPage
	(case NsCacheV2.lookup cache email
	   of SOME [lastname,firstnames] => "Name for " ^ email ^ 
	     " is: (" ^ firstnames ^ "," ^ lastname ^ ")"
	 | SOME _ => "Mega error in the internal cache representation!!!"
	 | NONE => "No name in cache for " ^ email)
	   


