
signature RATING_UTIL =
  sig
    val returnPage : string -> string frag list 
      -> Ns.status
      
    val returnPageWithTitle : string 
      -> string frag list -> Ns.status
      
    val bottleImgs : int -> string
      
    val mailto   : string -> string -> string
      
  (* 
   [returnPage title body] returns a page 
   to a browser.
   
   [returnPageWithTitle title body] returns a
   page to a browser with title as h1-header.
   
   [bottleImgs n] returns html code for n bottles.
   
   [mailto email name] returns mailto anchor. 
   *)
  end

structure RatingUtil : RATING_UTIL =
  struct
    fun returnPage title body =
      Ns.return (`
       <html> <title>^title</title>
       <body bgcolor=white>
       <center>` ^^ body ^^ 
        `<hr> <i>Served by <a 
         href=http://www.smlserver.org>SMLserver</a>
	 </i>
       </center>
       </body>
       </html>`)
      
    fun returnPageWithTitle title body =
      returnPage title (`<h1>^title</h1>` ^^ body)

    (* A procedure for generating bottle images *)
    fun bottleImgs n =
      let fun g (n, acc) = 
	    if n <= 0 then concat acc
	    else g (n - 1, "<img src=wine.jpg>" :: acc)
      in g(n,nil)
      end

    fun mailto email name = 
      "<a href=\"mailto:" ^ email ^ "\">" ^ name ^ "</a>"
  end
