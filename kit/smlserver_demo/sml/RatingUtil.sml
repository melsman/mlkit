(*
signature RATING_UTIL =
  sig
    type wid
    val widToString : wid -> string

    type wineRow = {name: string, year: int}
    type ratingRow = {wid: wid,
		      comment: string,
		      rate: int,
		      email: string,
		      name: string}

    val wineNew   : wineRow -> wid
    val ratingNew : ratingRow -> unit

    val wineShow : unit -> wineRow list
  end
*)

  signature RATING_UTIL =
    sig
      (* [returnPage title body] returns a page 
       * to a browser. *)
      val returnPage : string -> string frag list 
	-> Ns.status

      (* [returnPageWithTitle title body] returns a
       * page to a browser with title as h1-header. *)      
      val returnPageWithTitle : string 
	-> string frag list -> Ns.status

      (* [bottleImgs n] returns html code for n bottles. *)
      val bottleImgs : int -> string

      (* [mailto email name] returns mailto anchor. *)
      val mailto   : string -> string -> string
    end

structure RatingUtil : RATING_UTIL =
  struct
    fun returnPage title body =
      Ns.Quot.return `
       <html> <title>^title</title>
         <body bgcolor=white>
           <center>^(Ns.Quot.flatten body)
             <hr> <i>Served by SMLserver</i>
           </center>
         </body>
       </html>`
      
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
