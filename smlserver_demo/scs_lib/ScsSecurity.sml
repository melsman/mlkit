structure ScsSecurity (*:> SCS_SECURITY*) =
  struct
    fun randomChar () = 
      String.sub ("123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz./",
		  Random.range (0,61) (Random.newgen ()))

    (* [xssFilter text] returns a string which is more secure to present to user
       Taken from http://www.cgisecurity.com/articles/xss-faq.shtml
         Converting < and > to &lt; and &gt; is also suggested when it comes 
         to script output. Filtering < and > alone will not solve all cross site 
         scripting attacks and it is suggested you also attempt to filter out 
         ( and ) by translating them to &#40; and &#41;, 
         and also # and & by translating them to &#35 (#) and &#38 (&). 
     *)
    fun xssFilter (text:string) =
      let
	fun filter ch =
	  case ch of
	      #"<" => "&lt"
	    | #">" => "&gt"
	    | #"(" => "#40"
	    | #")" => "#41"
	    | #"#" => "#35"
	    | #"&" => "#38"
	    | _    => Char.toString ch
      in
        String.translate filter text
      end
  end