(** HTML combinators.

Combinators for constructing HTML elements. The library does not
enforce that the generated HTML conforms to the definition of any HTML
standard. It does, however, ensure that elements are wellformed.
*)

signature HTML = 
  sig
    type t                                 (* elements *)
    type attrs = (string * string) list    (* attributes *)

    (* General operations *)
    val &        : t * t -> t
    val $        : string -> t
    val seq      : t list -> t
    val toString : t -> string
    val comment  : t -> t

    (* HTML documents and headers *)
    val html     : t -> t
    val head     : t -> t
    val title    : t -> t
    val body     : t -> t
    val bodya    : attrs -> t -> t

    (* HTML headings and vertical format *)                                 
    val h1       : t -> t
    val h2       : t -> t
    val h3       : t -> t
    val h4       : t -> t
    val h5       : t -> t
    val h6       : t -> t
    val h1a      : attrs -> t -> t
    val h2a      : attrs -> t -> t
    val h3a      : attrs -> t -> t
    val h4a      : attrs -> t -> t
    val h5a      : attrs -> t -> t
    val h6a      : attrs -> t -> t
    val p        : t -> t
    val pa       : attrs -> t -> t
    val br       : t
    val bra      : attrs -> t
    val hr       : t
    val hra      : attrs -> t

    val divi        : t -> t
    val divia       : attrs -> t -> t
    val span        : attrs -> t -> t
    val blockquote  : t -> t
    val blockquotea : attrs -> t -> t
    val center      : t -> t
    val address     : t -> t
    val pre         : t -> t

    (* HTML anchors and hyperlinks *)
    val ahref    : string -> t -> t
    val ahrefa   : attrs -> string -> t -> t
    val aname    : string -> t -> t

    (* HTML text formats and style *)
    val em       : t -> t
    val strong   : t -> t
    val tt       : t -> t
    val sub      : t -> t
    val sup      : t -> t
                                 
    (* HTML lists *)
    val ul       : t -> t
    val ula      : attrs -> t -> t
    val ol       : t -> t
    val ola      : attrs -> t -> t
    val li       : t -> t
    val dl       : t -> t
    val dla      : attrs -> t -> t
    val dt       : t -> t
    val dd       : t -> t

    (* HTML tables *)
    val table    : t -> t
    val tablea   : attrs -> t -> t
    val tr       : t -> t
    val tra      : attrs -> t -> t
    val td       : t -> t
    val tda      : attrs -> t -> t
    val th       : t -> t
    val tha      : attrs -> t -> t
    val caption  : t -> t
    val captiona : attrs -> t -> t

    (* HTML images and image maps *)
    val img      : string -> t
    val imga     : attrs -> string -> t

    (* HTML forms etc *)
    val form       : string -> t -> t
    val forma      : attrs -> string -> t -> t
    val input      : string -> t
    val inputa     : string -> attrs -> t
    val intext     : string -> attrs -> t
    val inpassword : string -> attrs -> t
    val incheckbox : {name : string, value : string} -> attrs -> t
    val inradio    : {name : string, value : string} -> attrs -> t
    val inreset    : string -> attrs -> t
    val insubmit   : string -> attrs -> t
    val inhidden   : {name : string, value : string} -> t
    val textarea   : string -> t -> t
    val textareaa  : string -> attrs -> t -> t
    val select     : string -> attrs -> t -> t
    val option     : string -> t
                               
    (* HTML encoding  *)
    val urlencode  : string -> string
    val htmlencode : string -> string                          
  end

(** 

[t] is the common type for holding elements of any kind. The
implementation ensures that concatenation of elements using & and seq
is efficient. Constructing HTML, functionall, as a value of type t, is
more efficient than building it (functionally) as a string, and more
convenient and modular than building it (imperatively) by calling
print.

[$ s] represents the string s.

[e1 & e2] represents the concatenation of the elements represented by
e1 and e2.  The function & should be declared

  infix &

[toString e] returns the string represented by e.

[comment e] generates  the special element <!--e-->.

[html e] generates <html>e</html>.

[head e] generates <head>e</head>.

[title e] generates <title>e</title>.

[body e] generates <body>e</body>.

[bodya attrs e] generates <body attrs>e</body>.

[h1 e] generates <h1>e</h1>.

[p e] generates <p>e</p>.

[pa attrs e] generates <p attrs>e</p>.

[br] generates <br/>.

[bra attrs] generates <br attrs/>.

[hr] generates <hr/>.

[hra attrs] generates <hr attrs>.

[divi e] generates <div>e</div>.

[divia attrs e] generates <div attrs>e</div>.

[span attrs e] generates <span attrs>e</span>

[blockquote e] generates <blockquote>e</blockquote>.

[blockquotea attrs e] generates <blockquote attrs>e</blockquote> 

[center e] generates <center>e</center>.

[address e] generates <address>e</address>.

[pre e] generates <pre>e</pre>.

[ahref link e] generates <a href='link'>e</a>.

[ahrefa link attrs e] generates <a href='link' attrs>e</a>.

[aname nam e] generates <a name='name'>e</a>.

[em e] generates <em>e</em>.

[strong e] generates <strong>e</strong>.

[tt e] generates <tt>e</tt>.

[sub e] generates <sub>e</sub>.

[sup e] generates <sup>e</sup>.

[ul e] generates <ul>e</ul>.

[ula attrs e] generates <ul attrs>e</ul>.

[ol e] generates <ol>e</ol>.

[ola attrs e] generates <ol attrs>e</ol>.

[li e] generates <li>e</li>.

[dl e] generates <dl>e</dl>.

[dla attrs e] generates <dl attrs>e</dl>.

[dt e] generates <dt>e</dt>.

[dd e] generates <dd>e</dd>.

[table e] generates <table>e</table>.

[tablea attrs e] generates <table attrs>e</table>.

[tr e] generates <tr>e</tr>.

[tra attrs e] generates <tr attrs>e</tr>.

[td e] generates <td>e</td>.

[tda attrs e] generates <td attrs>e</td>.

[th e] generates <th>e</th>.

[tha attrs e] generates <th attrs>e</th>.

[caption e] generates <caption>e</caption>.

[captiona attrs e] generates <caption attrs>e</caption>.

[img s] generates <img src='s'>.

[imga s attrs] generates <img src='s' attrs>.

[form act e] generates <form action='act'>e</form>.

[forma act attrs e] generates <form action='act' attrs>e</form>.

[input typ] generates <input type='typ'/>.

[inputa typ attrs] generates <input type='typ' attrs/>.

[intext name attrs] generates <input type='text' name='name' attrs/>.

[inpassword name attrs] generates <input type='password' name='name'
attrs/>.

[incheckbox {name, value} attrs] generates <input type='checkbox'
name='name' value='value' attrs/>.

[inradio {name, value} attrs] generates <input type='radio'
name='name' value='value' attrs/>.

[inreset value attrs] generates <input type='reset' value='value' attrs/>.

[insubmit value attrs] generates <input type='submit' value='value' attrs/>.

[inhidden {name, value}] generates <input type='hidden' name='name'
value='value'/>.

[textarea name e] generates <textarea name='name'>e</textarea>.

[textareaa name attrs e] generates <textarea name='name'
attrs>e</textarea>.

[select name attrs e] generates <select name='name' attrs>e</select>.

[option value] generates <option value='value'/>.

[urlencode s] returns the url-encoding of s.  That is, space (ASCII
32) is replaced by `+' and every non-alphanumeric character c except
the characters - _ . is replaced by %hh, where hh is the hexadecimal
representation of the ASCII code of c.

[htmlencode s] returns the html-encoding of s.  That is, < and >
are replaced by &lt; and &gt; respectively, and & is replaced by 
&amp;
*)
