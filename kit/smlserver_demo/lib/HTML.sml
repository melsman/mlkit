signature HTML = 
  sig
    (* HTML generic marks *)
    val mark0    : string -> quot
    val mark0a   : string -> string -> quot
    val mark1    : string -> quot -> quot
    val mark1a   : string -> string -> quot -> quot
    val comment  : string -> quot

    (* HTML documents and headers *)
    val html     : quot -> quot
    val head     : quot -> quot
    val title    : quot -> quot
    val body     : quot -> quot
    val bodya    : string -> quot -> quot
    val htmldoc  : quot -> quot -> quot

    (* HTML headings and vertical format *)
    val h1       : quot -> quot
    val h2       : quot -> quot
    val h3       : quot -> quot
    val h4       : quot -> quot
    val h5       : quot -> quot
    val h6       : quot -> quot
    val p        : quot -> quot
    val pa       : string -> quot -> quot
    val br       : quot
    val bra      : string -> quot
    val hr       : quot
    val hra      : string -> quot

    val divi        : quot -> quot
    val divia       : string -> quot -> quot
    val blockquote  : quot -> quot
    val blockquotea : string -> quot -> quot
    val center      : quot -> quot
    val address     : quot -> quot
    val pre         : quot -> quot

    (* HTML anchors and hyperlinks *)
    val ahref    : quot -> quot -> quot
    val ahrefa   : quot -> quot -> quot -> quot
    val aname    : string -> quot -> quot
    val aemail   : string -> string -> quot

    (* HTML text formats and style *)
    val em       : quot -> quot
    val strong   : quot -> quot
    val tt       : quot -> quot
    val sub      : quot -> quot
    val sup      : quot -> quot
    val fonta    : string -> quot -> quot

    (* HTML lists *)
    val ul       : quot -> quot
    val ula      : string -> quot -> quot
    val ol       : quot -> quot
    val ola      : string -> quot -> quot
    val li       : quot -> quot
    val dl       : quot -> quot
    val dla      : string -> quot -> quot
    val dt       : quot -> quot
    val dd       : quot -> quot

    (* HTML tables *)
    val table    : quot -> quot
    val tablea   : string -> quot -> quot
    val tr       : quot -> quot
    val tra      : string -> quot -> quot
    val td       : quot -> quot
    val tda      : string -> quot -> quot
    val th       : quot -> quot
    val tha      : string -> quot -> quot
    val caption  : quot -> quot
    val captiona : string -> quot -> quot

    (* HTML images and image maps *)
    val img      : string -> quot
    val imga     : string -> string -> quot
    val map      : string -> quot -> quot
    val mapa     : string -> string -> quot -> quot
    val area     : {alt : string option, coords : string, 
		    href : string option, shape : string} -> quot

    (* HTML forms etc *)
    val form       : string -> quot -> quot
    val forma      : string -> string -> quot -> quot
    val input      : string -> quot
    val inputa     : string -> string -> quot
    val intext     : string -> quot -> quot
    val inpassword : string -> string -> quot
    val incheckbox : {name : string, value : string} -> string -> quot
    val inradio    : {name : string, value : string} -> string -> quot
    val inreset    : string -> string -> quot
    val insubmit   : string -> string -> quot
    val inhidden   : string -> string -> quot
    val textarea   : string -> quot -> quot
    val textareaa  : string -> string -> quot -> quot
    val select     : string -> string -> quot -> quot
    val option     : string -> string -> quot

    val export_hiddens : (string * string) list -> quot
    val export_url_vars: (string * string) list -> quot
    val genUrl         : string -> (string * string) list -> string
    val genUrla        : string -> (string * string) list -> string -> string
    (* HTML frames and framesets *)
    val frameset   : string -> quot -> quot
    val frame      : { src : string, name : string } -> quot
    val framea     : { src : string, name : string } -> string -> quot

    (* HTML encoding  *)
    val urlencode  : string -> string
    val htmlencode : quot -> quot
end

(* 
   This module provides HTML support functions for SMLserver scripts

   HTML generic marks:

   [mark0 t] generates the HTML tag <t> as a quot.

   [mark0a attr t] generates the attributed HTML tag <t attr> as a quot.

   [mark1 t ws] generates  <t>ws</t>  as a quot.

   [mark1a attr t ws] generates  <t attr>ws</t> as a quot.

   [comment ws] generates  <!--ws-->  as a quot.


   HTML documents and headers:

   [html ws] generates <HTML>ws</HTML>.

   [head ws] generates <HEAD>ws</HEAD>.

   [title ws] generates <TITLE>ws</TITLE>.

   [body ws] generates <BODY>ws</BODY>.

   [bodya attr ws] generates <BODY attr>ws</BODY>.

   [htmldoc titl ws] generates 
   <HTML><HEAD><TITLE>titl</TITLE></HEAD><BODY>ws</BODY></HTML>.

   
   HTML headings and vertical format:

   [h1 ws] generates <H1>ws</H1>.

   [p ws] generates <P>ws</P>.

   [pa attr ws] generates <P attr>ws</P>.

   [br] generates <BR>.

   [bra attr] generates <BR attr>.

   [hr] generates <HR>.

   [hra attr] generates <HR attr>.

   [divi ws] generates <DIV>ws</DIV>.

   [divia attr ws] generates <DIV attr>ws</DIV>.

   [blockquote ws] generates <BLOCKQUOTE>ws</BLOCKQUOTE>.

   [blockquotea attr ws] generates <BLOCKQUOTE attr>ws</BLOCKQUOTE> 

   [center ws] generates <CENTER>ws</CENTER>.

   [address ws] generates <ADDRESS>ws</ADDRESS>.

   [pre ws] generates <PRE>ws</PRE>.


   HTML anchors and hyperlinks:

   [ahref link ws] generates <A HREF="link">ws</A>.

   [ahrefa link attr ws] generates <A HREF="link" attr>ws</A>.

   [aname nam ws] generates <A NAME="name">ws</A>.

   [aemail email name] generate <a href="mailto:email">name</a>.

   HTML text formats and style:

   [em ws] generates <EM>ws</EM>.

   [strong ws] generates <STRONG>ws</STRONG>.

   [tt ws] generates <TT>ws</TT>.

   [sub ws] generates <SUB>ws</SUB>.

   [sup ws] generates <SUP>ws</SUP>.

   [fonta attr ws] generates <FONT attr>ws</FONT>.


   HTML lists:

   [ul ws] generates <UL>ws</UL>.

   [ula attr ws] generates <UL attr>ws</UL>.

   [ol ws] generates <OL>ws</OL>.

   [ola attr ws] generates <OL attr>ws</OL>.

   [li ws] generates <LI>ws</LI>.

   [dl ws] generates <DL>ws</DL>.

   [dla attr ws] generates <DL attr>ws</DL>.

   [dt ws] generates <DT>ws</DT>.

   [dd ws] generates <DD>ws</DD>.


   HTML tables:

   [table ws] generates <TABLE>ws</TABLE>.

   [tablea attr ws] generates <TABLE attr>ws</TABLE>.

   [tr ws] generates <TR>ws</TR>.

   [tra attr ws] generates <TR attr>ws</TR>.

   [td ws] generates <TD>ws</TD>.

   [tda attr ws] generates <TD attr>ws</TD>.

   [th ws] generates <TH>ws</TH>.

   [tha attr ws] generates <TH attr>ws</TH>.

   [caption ws] generates <CAPTION>ws</CAPTION>.

   [captiona attr ws] generates <CAPTION attr>ws</CAPTION>.


   HTML images and image maps:

   [img s] generates <IMG SRC="s">.

   [imga s attr] generates <IMG SRC="s" attr>.

   [map nam ws] generates <MAP NAME="name">ws</MAP>.

   [mapa nam attr ws] generates <MAP NAME="name" attr>ws</MAP>.

   [area { alt, coords, href, shape}] generates
       <AREA SHAPE="shape" COORDS="coords" HREF="link" ALT="desc"> 
   when href is SOME link (where HREF is replaced by NOHREF otherwise)
   and  alt  is SOME desc (where ALT is omitted otherwise).


   HTML forms etc:

   [form act ws] generates <FORM ACTION="act">ws</FORM>.

   [forma act attr ws] generates <FORM ACTION="act" attr>ws</FORM>.

   [input typ] generates <INPUT TYPE=typ>.

   [inputa typ attr] generates <INPUT TYPE=typ attr>.

   [intext name attr] generates <INPUT TYPE=TEXT NAME="name" attr>.

   [inpassword name attr] generates <INPUT TYPE=PASSWORD NAME="name" attr>.

   [incheckbox {name, value} attr] generates 
   <INPUT TYPE=CHECKBOX NAME="name" VALUE="value" attr>.

   [inradio {name, value} attr] generates 
   <INPUT TYPE=RADIO NAME="name" VALUE="value" attr>.

   [inreset value attr] generates <INPUT TYPE=RESET VALUE="value" attr>.

   [insubmit value attr] generates <INPUT TYPE=SUBMIT VALUE="value" attr>.

   [inhidden name value] generates
   <INPUT TYPE=HIDDEN NAME="name" VALUE="value">.

   [textarea name ws] generates <TEXTAREA NAME="name">ws</TEXTAREA>.

   [textareaa name attr ws] generates 
   <TEXTAREA NAME="name" attr>ws</TEXTAREA>.

   [select name attr ws] generates <SELECT NAME="name" attr>ws</SELECT>.

   [option value] generates <OPTION VALUE="value">.


   HTML frames and framesets:

   [frameset attr ws] generates <FRAMESET attr>ws</FRAMESET>.

   [frame { src, name }] generates <FRAME SRC="src" NAME="name">.

   [framea { src, name } attr] generates <FRAME SRC="src" NAME="name" attr>.


   HTML encoding functions:

   [urlencode s] returns the url-encoding of s.  That is, space (ASCII 32) 
   is replaced by `+' and every non-alphanumeric character c except 
   the characters - _ . is replaced by %hh, where hh is the hexadecimal 
   representation of the ASCII code of c.

   [htmlencode s] returns the html-encoding of s.  That is, < and >
   are replaced by &lt; and &gt; respectively, and & is replaced by 
   &amp;
*)
