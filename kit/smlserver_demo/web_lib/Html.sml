structure Html :> HTML = 
  struct
    (* HTML generic marks *)
    fun mark0 tag = `<^tag>`
    fun mark0a attr tag = `<^tag ^attr>`
    fun mark1 tag quot = `<^tag>` ^^ quot ^^ `</^tag>`
    fun mark1a tag attr quot = `<^tag ^attr>` ^^ quot ^^ `</^tag>`
    fun comment str = `<!-- ^str -->`

    (* HTML documents and headers *)
    fun html quot = `<html>` ^^ quot ^^ `</html>`
    fun head quot = `<head>` ^^ quot ^^ `</head>`
    fun title quot = `<title>` ^^ quot ^^ `</title>`
    fun body quot = `<body>` ^^ quot ^^ `</body>`
    fun bodya attr quot = ` <body ^attr>` ^^ quot ^^ `</body>`
    fun htmldoc tit bod = html (head (title tit) ^^ body bod)

    (* HTML headings and vertical format *)
    fun h1 quot = `<h1>` ^^ quot ^^ `</h1>`
    fun h2 quot = `<h2>` ^^ quot ^^ `</h2>`
    fun h3 quot = `<h3>` ^^ quot ^^ `</h3>`
    fun h4 quot = `<h4>` ^^ quot ^^ `</h4>`
    fun h5 quot = `<h5>` ^^ quot ^^ `</h5>`
    fun h6 quot = `<h6>` ^^ quot ^^ `</h6>`

    fun p quot = `<p>` ^^ quot ^^ `</p>`
    fun pa attr quot = `<p ^attr>` ^^ quot ^^ `</p>`
    val br = `<br>`
    fun bra attr = `<br ^attr>`
    val hr = `<hr>`
    fun hra attr = `<hr ^attr>`

    fun divi quot = `<div>` ^^ quot ^^ `</div>`
    fun divia attr quot = `<div ^attr>` ^^ quot ^^ `</div>`
    fun blockquote quot = `<blockquote>` ^^ quot ^^ `</blockquote>`
    fun blockquotea attr quot = `<blockquote ^attr>` ^^ quot ^^ `</blockquote>`
    fun center quot = `<center>` ^^ quot ^^ `</center>`
    fun address quot = `<address>` ^^ quot ^^ `</address>`
    fun pre quot = `<pre>` ^^ quot ^^ `</pre>`

    (* HTML anchors and hyperlinks *)
    fun ahref link name = `<a href="` ^^ link ^^ `">` ^^ name ^^ `</a>`
    fun ahrefa link attr name = `<a href="` ^^ link ^^ `" ` ^^ attr ^^ `>` ^^ name ^^ `</a>`
    fun aname name quot = `<a name="^name">` ^^ quot ^^ `</a>`
    fun aemail email name = `<a href="mailto:^email">^name</a>`

    (* HTML text formats and style *)
    fun em quot = `<em>` ^^ quot ^^ `</em>`
    fun strong quot = `<strong>` ^^ quot ^^ `</strong>`
    fun tt quot = `<tt>` ^^ quot ^^ `</tt>`
    fun sub quot = `<sub>` ^^ quot ^^ `</sub>`
    fun sup quot = `<sup>` ^^ quot ^^ `</sup>`
    fun fonta attr quot = `<font ^attr>` ^^ quot ^^ `</font>`

    (* HTML lists *)
    fun ul quot = `<UL>` ^^ quot ^^ `</UL>`
    fun ula attr quot = `<UL ^attr>` ^^ quot ^^ `</UL>`
    fun ol quot = `<OL>` ^^ quot ^^ `</OL>`
    fun ola attr quot = `<OL ^attr>` ^^ quot ^^ `</OL>`
    fun li quot = `<LI>` ^^ quot ^^ `</LI>`

    fun dl quot = `<DL>` ^^ quot ^^ `</DL>`
    fun dla attr quot = `<DL ^attr>` ^^ quot ^^ `</DL>`
    fun dt quot = `<DT>` ^^ quot ^^ `</DT>`
    fun dd quot = `<DD>` ^^ quot ^^ `</DD>`

    (* HTML tables *)
    fun tr quot = `<TR>` ^^ quot ^^ `</TR>`
    fun tra attr quot = `<TR ^attr>` ^^ quot ^^ `</TR>`
    fun td quot = `<TD>` ^^ quot ^^ `</TD>`
    fun tda attr quot = `<TD ^attr>` ^^ quot ^^ `</TD>`
    fun th quot = `<TH>` ^^ quot ^^ `</TH>`
    fun tha attr quot = `<TH ^attr>` ^^ quot ^^ `</TH>`
    fun table quot = `<TABLE>` ^^ quot ^^ `</TABLE>`
    fun tablea attr quot = `<TABLE ^attr>` ^^ quot ^^ `</TABLE>`
    fun caption quot = `<CAPTION>` ^^ quot ^^ `</CAPTION>`
    fun captiona attr quot = `<CAPTION ^attr>` ^^ quot ^^ `</CAPTION>`

    (* HTML images and image maps *)
    fun img src  = `<IMG SRC="^src">`
    fun imga src attr = `<IMG SRC="^src" ^attr>`
    fun map name quot = `<MAP NAME="^name">` ^^ quot ^^ `</MAP>`
    fun mapa name attr quot = `<MAP NAME="^name" ^attr>` ^^ quot ^^ `</MAP>`
    fun area { shape, href, coords, alt } = 
      `<AREA SHAPE="^shape" COORDS="^coords" ` ^^ 
      (case href of NONE => `NOHREF` | SOME r => `HREF="^r" `) ^^ 
	 (case alt  of NONE => `` | SOME a => `ALT="^a"`) ^^ `>`

    (* HTML forms etc *)
    fun form action quot = `<FORM ACTION="^action">` ^^ quot ^^ `</FORM>`
    fun forma action attr quot = `<FORM ACTION="^action" ^attr>` ^^ quot ^^ `</FORM>`
    fun input typ = `<INPUT TYPE="^typ">`
    fun inputa typ attr = `<INPUT TYPE="^typ" ^attr>`
    fun intext name attr = `<input type="text" name="^name" ` ^^ attr ^^ `>`
    fun inpassword name attr = `<INPUT TYPE=PASSWORD NAME="^name" ^attr>`
    fun incheckbox {name, value} attr = `<INPUT TYPE=CHECKBOX VALUE="^value" NAME="^name" ^attr>`
    fun inradio {name, value} attr = `<INPUT TYPE=RADIO VALUE="^value" NAME="^name" ^attr>`
    fun inreset value attr = `<INPUT TYPE=RESET VALUE="^value" ^attr>`
    fun insubmit value attr = `<INPUT TYPE=SUBMIT VALUE="^value" ^attr>`
    fun inhidden name value = `<INPUT TYPE=HIDDEN NAME="^name" VALUE="^value">`
    fun textarea name quot = `<TEXTAREA NAME="^name">` ^^ quot ^^ `</TEXTAREA>`
    fun textareaa name attr quot = `<TEXTAREA NAME="^name" ^attr>` ^^ quot ^^ `</TEXTAREA>`
    fun select name attr quot = `<SELECT NAME="^name" ^attr>` ^^ quot ^^ `</SELECT>`
    fun option value name = `<OPTION VALUE="^value">^name</OPTION>`

    (* Type of an url consists of a file part and form variables *)
    type fv     = string
    type fv_val = string
    type fvs    = (fv * fv_val) list
    type url    = {file: string, fvs: fvs}
    fun getFileFromUrl (url : url) = #file url
    fun getFvsFromUrl (url : url) = #fvs url
    fun buildUrl file fvs = {file=file,fvs=fvs}
    fun export_hiddens hvs =
      (List.foldr (fn ((n,v),acc) => `
		   <input type=hidden name="^n" value="^v">` ^^ acc) `` hvs)
    fun export_url_vars [] = ``
      | export_url_vars hvs = `?` ^^ (Quot.concatWith "&" (List.map (fn (n,v) => `^(n)=^(Web.encodeUrl v)`) hvs))
    fun genUrl u hvs = Quot.toString (`^u` ^^ (export_url_vars hvs))
    fun genUrla u hvs a = Quot.toString (`^u` ^^ (export_url_vars hvs) ^^ `^a`)
    fun flattenUrl (url : url) = genUrl (getFileFromUrl url) (getFvsFromUrl url)

    (* HTML frames and framesets *)
    fun frameset attr quot = `<FRAMESET ^attr` ^^ quot ^^ `</FRAMESET>`
    fun frame { src, name } = `<FRAME SRC="^src" NAME="^name">` 
    fun framea { src, name } attr = `<FRAME SRC="^src" NAME="^name" ^attr>` 

    (* HTML encoding *)

    fun urlencode s : string =
      let fun encode #" " = "+"
	    | encode #"-" = "-"
	    | encode #"_" = "_"
	    | encode #"." = "."
	    | encode c    = 
	if Char.isAlphaNum c then String.str c 
	else "%" ^ StringCvt.padLeft #"0" 2 
	  (Int.fmt StringCvt.HEX (Char.ord c))
      in String.translate encode s end

    fun htmlencode q : quot =
      let fun encode #"<" = "&lt;"
	    | encode #">" = "&gt;"
	    | encode #"&" = "&amp;"
	    | encode c    = String.str c
      in Quot.translate encode q end

    fun convertTags (text:string) =
      let
	val ltList = [#"<"]
	val newline = #"\n"
	val space = #" "
	val star = #"*"
        fun parseTag (text,stack:(char list),remove_p) = 
          let
            val replaceLT = (rev(ltList @ ((tl o rev) stack))) 
            val removeLT = (rev(((tl o rev) stack))) 
	  in 
            case (Substring.getc text) of
	      SOME(c,rest) => ( case (Char.toLower c) of
		  #"b"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		          #">"  => ( rest1, removeLT )
		        | #"r"  => ( case (Substring.getc rest1) of
			      SOME(c2,rest2) => ( case c2 of
			        #">"  => ( rest2, if remove_p then removeLT else newline::removeLT )
			        | other => ( rest2, other::c1::c::replaceLT )
			      )
			      | NONE => ( rest1, c1::c::replaceLT )
                          )
		        | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
		| #"e"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		        #"m"  => ( case (Substring.getc rest1) of
			  SOME(c2,rest2) => ( case c2 of
			    #">"  => ( rest2, removeLT )
			  | other => ( rest2, other::c1::c::replaceLT )
			  )
			| NONE => ( rest1, c1::c::replaceLT )
                        )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
		| #"i"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case c1 of
		        #">"  => ( rest1, removeLT )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
		  )
		| #"l"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		        #"i"  => ( case (Substring.getc rest1) of
			  SOME(c2,rest2) => ( case c2 of
			    #">"  => ( rest2, if remove_p then removeLT else space::star::space::space::newline::removeLT )
			  | other => ( rest2, other::c1::c::replaceLT )
			  )
			| NONE => ( rest1, c1::c::replaceLT )
                        )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
		| #"o"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		        #"l"  => ( case (Substring.getc rest1) of
			  SOME(c2,rest2) => ( case c2 of
			    #">"  => ( rest2, removeLT )
			  | other => ( rest2, other::c1::c::replaceLT )
			  )
			| NONE => ( rest1, c1::c::replaceLT )
                        )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
		| #"p"  => ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case c1 of
		        #">"  => ( rest1, newline::removeLT )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
		  )
		| #"t"  =>  ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		        #"t"  => ( case (Substring.getc rest1) of
			  SOME(c2,rest2) => ( case c2 of
			    #">"  => ( rest2, removeLT )
			  | other => ( rest2, other::c1::c::replaceLT )
			  )
			| NONE => ( rest1, c1::c::replaceLT )
                        )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
		| #"u"  =>  ( case (Substring.getc rest) of
  	              SOME(c1,rest1) => ( case (Char.toLower c1) of
		        #"l"  => ( case (Substring.getc rest1) of
			  SOME(c2,rest2) => ( case c2 of
			    #">"  => ( rest2, removeLT )
			  | other => ( rest2, other::c1::c::replaceLT )
			  )
			| NONE => ( rest1, c1::c::replaceLT )
                        )
		      | other => ( rest1, other::c::replaceLT )
                      )
 		    | NONE => ( rest, c::replaceLT )
                  )
                | other => ( rest, c::replaceLT )
              )
              | NONE => ( text, replaceLT )
          end

        fun parseHTML (substr) (acc:(char list)) = case (Substring.getc substr) of
            SOME(c,rest) => (case c of
		#"<" => ( case (Substring.first rest) of
                    SOME c1 => 
		      let 
                        val (rest1, stack) = 
			  if c1 = #"/" then 
                            let val (c1,rest1)= (valOf o Substring.getc) rest 
			    in  parseTag (rest1,[c],true) end
			else 
			  parseTag (rest,[c],false)
                      in 			
			parseHTML rest1 (stack@acc)
		      end

                  | NONE => parseHTML rest ( (rev ltList) @ acc )
                )
	      | other => parseHTML rest ( other::acc )

            )
	  | NONE => implode (rev acc) 
      in
        parseHTML (Substring.all text) []
      end


  end (* of structure *)


