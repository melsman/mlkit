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
val _ = Web.Cache.flush(cache)

val _ = Page.return ("Cache Demonstration" ^": cache_flush.sml")
  (`The cache has been flushed.
  
  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)
