structure Html :> HTML = 
  struct
    (* HTML generic marks *)
    fun mark0 tag = `<^tag>`
    fun mark0a attr tag = `<^tag ^attr>`
    fun mark1 tag quot = `<^tag>` ^^ quot ^^ `</^tag>`
    fun mark1a tag attr quot = `<^tag ^attr>` ^^ quot ^^ `</^tag>`
    fun comment str = `<!-- ^str -->`

    (* HTML documents and headers *)
    fun html quot = `<HTML>` ^^ quot ^^ `</HTML>`
    fun head quot = `<HEAD>` ^^ quot ^^ `</HEAD>`
    fun title quot = `<TITLE>` ^^ quot ^^ `</TITLE>`
    fun body quot = `<BODY>` ^^ quot ^^ `</BODY>`
    fun bodya attr quot = ` <BODY ^attr>` ^^ quot ^^ `</BODY>`
    fun htmldoc tit bod = html (head (title tit) ^^ body bod)

    (* HTML headings and vertical format *)
    fun h1 quot = `<H1>` ^^ quot ^^ `</H1>`
    fun h2 quot = `<H2>` ^^ quot ^^ `</H2>`
    fun h3 quot = `<H3>` ^^ quot ^^ `</H3>`
    fun h4 quot = `<H4>` ^^ quot ^^ `</H4>`
    fun h5 quot = `<H5>` ^^ quot ^^ `</H5>`
    fun h6 quot = `<H6>` ^^ quot ^^ `</H6>`

    fun p quot = `<P>` ^^ quot ^^ `</P>`
    fun pa attr quot = `<P ^attr>` ^^ quot ^^ `</P>`
    val br = `<BR>`
    fun bra attr = `<BR ^attr>`
    val hr = `<HR>`
    fun hra attr = `<HR ^attr>`

    fun divi quot = `<DIV>` ^^ quot ^^ `</DIV>`
    fun divia attr quot = `<DIV ^attr>` ^^ quot ^^ `</DIV>`
    fun blockquote quot = `<BLOCKQUOTE>` ^^ quot ^^ `</BLOCKQUOTE>`
    fun blockquotea attr quot = `<BLOCKQUOTE ^attr>` ^^ quot ^^ `</BLOCKQUOTE>`
    fun center quot = `<CENTER>` ^^ quot ^^ `</CENTER>`
    fun address quot = `<ADDRESS>` ^^ quot ^^ `</ADDRESS>`
    fun pre quot = `<PRE>` ^^ quot ^^ `</PRE>`

    (* HTML anchors and hyperlinks *)
    fun ahref link name = `<A HREF="` ^^ link ^^ `">` ^^ name ^^ `</A>`
    fun ahrefa link attr name = `<A HREF="` ^^ link ^^ `" ` ^^ attr ^^ `>` ^^ name ^^ `</A>`
    fun aname name quot = `<A NAME="^name">` ^^ quot ^^ `</A>`

    (* HTML text formats and style *)
    fun em quot = `<EM>` ^^ quot ^^ `</EM>`
    fun strong quot = `<STRONG>` ^^ quot ^^ `</STRONG>`
    fun tt quot = `<TT>` ^^ quot ^^ `</TT>`
    fun sub quot = `<SUB>` ^^ quot ^^ `</SUB>`
    fun sup quot = `<SUP>` ^^ quot ^^ `</SUP>`
    fun fonta attr quot = `<FONT ^attr>` ^^ quot ^^ `</FONT>`

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
    fun intext name attr = `<INPUT TYPE=TEXT NAME="^name" ` ^^ attr ^^ `>`
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

    fun export_hiddens hvs =
      (List.foldr (fn ((n,v),acc) => `
		   <input type=hidden name="^n" value="^v">` ^^ acc) `` hvs)
    fun export_url_vars [] = ``
      | export_url_vars hvs = `?` ^^ (Quot.concatWith "&" (List.map (fn (n,v) => `^(n)=^(Ns.encodeUrl v)`) hvs))
    fun genUrl u hvs = Quot.toString (`^u` ^^ (export_url_vars hvs))

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

  end


