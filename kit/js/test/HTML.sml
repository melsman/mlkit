(* HTML combinators *)

(* Efficiently concatenable word sequences *)

signature HTML = sig

datatype html = 
    Empty                               (* The empty sequence         *)
  | Nl                                  (* Newline                    *)
  | $ of string                         (* A string                   *)
  | $$ of string list                   (* A sequence of strings      *)
  | && of html * html;                  (* Concatenation of sequences *)

(* Manipulating htmls *)

val prmap    : ('a -> html) -> 'a list -> html
val prsep    : html -> ('a -> html) -> 'a list -> html
val flatten  : html -> string
val printhtml : html -> unit
val vec2list : 'a vector -> 'a list

(* HTML generic marks *)

val mark0    : string -> html
val mark0a   : string -> string -> html
val mark1    : string -> html -> html
val mark1a   : string -> string -> html -> html
val comment  : html -> html

(* HTML documents and headers *)

val html     : html -> html
val head     : html -> html
val title    : html -> html
val body     : html -> html
val bodya    : string -> html -> html
val htmldoc  : html -> html -> html

(* HTML headings and vertical format *)

val h1       : html -> html
val h2       : html -> html
val h3       : html -> html
val h4       : html -> html
val h5       : html -> html
val h6       : html -> html
val p        : html -> html
val pa       : string -> html -> html
val br       : html
val bra      : string -> html
val hr       : html
val hra      : string -> html

val divi        : html -> html
val divia       : string -> html -> html
val blockquote  : html -> html
val blockquotea : string -> html -> html
val center      : html -> html
val address     : html -> html
val pre         : html -> html

(* HTML anchors and hyperlinks *)

val ahref    : string -> html -> html
val ahrefa   : string -> string -> html -> html
val aname    : string -> html -> html

(* HTML text formats and style *)

val em       : html -> html
val strong   : html -> html
val tt       : html -> html
val sub      : html -> html
val sup      : html -> html
val fonta    : string -> html -> html

(* HTML lists *)

val ul       : html -> html
val ula      : string -> html -> html
val ol       : html -> html
val ola      : string -> html -> html
val li       : html -> html
val dl       : html -> html
val dla      : string -> html -> html
val dt       : html -> html
val dd       : html -> html

(* HTML tables *)

val table    : html -> html
val tablea   : string -> html -> html
val tr       : html -> html
val tra      : string -> html -> html
val td       : html -> html
val tda      : string -> html -> html
val th       : html -> html
val tha      : string -> html -> html
val caption  : html -> html
val captiona : string -> html -> html

(* HTML images and image maps *)

val img      : string -> html
val imga     : string -> string -> html
val map      : string -> html -> html
val mapa     : string -> string -> html -> html
val area     : { alt : string option, coords : string, 
                 href : string option, shape : string} -> html

(* HTML forms etc *)

val form       : string -> html -> html
val forma      : string -> string -> html -> html
val input      : string -> html
val inputa     : string -> string -> html
val intext     : string -> string -> html
val inpassword : string -> string -> html
val incheckbox : {name : string, value : string} -> string -> html
val inradio    : {name : string, value : string} -> string -> html
val inreset    : string -> string -> html
val insubmit   : string -> string -> html
val inhidden   : {name : string, value : string} -> html
val textarea   : string -> html -> html
val textareaa  : string -> string -> html -> html
val select     : string -> string -> html -> html
val option     : string -> html

(* HTML frames and framesets *)

val frameset   : string -> html -> html
val frame      : { src : string, name : string } -> html
val framea     : { src : string, name : string } -> string -> html

(* HTML encoding  *)

val urlencode  : string -> string
val htmlencode : string -> string

end

(* 
   This module provides support functions for writing CGI scripts and
   ML Server Page scripts.

   [html] is the type of efficiently concatenable word sequences.
   Building an HTML page (functionally) as a html is more efficient
   than building it (functionally) as a string, and more convenient
   and modular than building it (imperatively) by calling print.

   [Empty] represents the empty string "".

   [Nl] represents the string "\n" consisting of a single newline character.

   [$ s] represents the string s.

   [$$ ss] represents the string String.concat(ss).

   [&&(ws1, ws2)] represents the concatenation of the strings
   represented by ws1 and ws2.  The function && should be declared
        infix &&

   [prmap f xs] is f x1 && ... && f xn evaluated from left to right, 
   when xs is [x1, ..., xn].

   [prsep sep f xs] is f x1 && sep && ... && sep && f xn, evaluated
   from left to right, when xs is [x1, ..., xn].

   [flatten ws] is the string represented by ws.

   [printhtml ws] is equivalent to print(flatten ws), but avoids
   building any new strings.

   [vec2list vec] is a list of the elements of vector vec.  Use it to
   convert e.g. the results of a database query into a list, for
   processing with prmap or prsep.


   HTML generic marks:

   [mark0 t] generates the HTML tag <t> as a html.

   [mark0a attr t] generates the attributed HTML tag <t attr> as a html.

   [mark1 t ws] generates  <t>ws</t>  as a html.

   [mark1a attr t ws] generates  <t attr>ws</t> as a html.

   [comment ws] generates  <!--ws-->  as a html.


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

   [inhidden {name, value}] generates
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
