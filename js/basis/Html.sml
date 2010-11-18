structure Html :> HTML = struct

datatype t = $ of string | seq of t list
type attrs = (string * string) list    (* attributes *)

(* General operations *)
infix &
fun e1 & e2 = seq [e1,e2]

fun toString t =
    let fun loop ($ s, acc) = s :: acc
          | loop (seq es, acc) = loops (es, acc)
        and loops (nil, acc) = acc
          | loops (e::es,acc) = loop (e,loops (es,acc))
    in String.concat (loop (t, nil))
    end
fun comment e = $"<!-- " & e & $ " -->"


fun attrs nil = $""
  | attrs ((n,v)::a) = $" " & $n & $"='" & $v & $"'" & attrs a
fun elma0 t a = seq[$"<", $t, attrs a, $"/>"]
fun elm0 t = elma0 t nil

fun elma t a e = seq[$"<", $t, attrs a, $">", e, $"</", $t, $">"]
fun elm t e = elma t nil e

(* HTML documents and headers *)
val html  = elm "html"
val head  = elm "head"
val title = elm "title"
val body  = elm "body"
val bodya = elma "body"

(* HTML headings and vertical format *)                                 
val h1 = elm "h1"
val h2 = elm "h2"
val h3 = elm "h3"
val h4 = elm "h4"
val h5 = elm "h5"
val h6 = elm "h6"
val h1a = elma "h1"
val h2a = elma "h2"
val h3a = elma "h3"
val h4a = elma "h4"
val h5a = elma "h5"
val h6a = elma "h6"
val p  = elm "p"   
val pa = elma "p"  
val br = elm0 "br" 
val bra= elma0 "br"
val hr = elm0 "hr" 
val hra= elma0 "hr"

val divi        = elm "div"
val divia       = elma "div"
val span        = elma "span"
val blockquote  = elm "blockquote"
val blockquotea = elma "blockquote"
val center      = elm "center"
val address     = elm "address"
val pre         = elm "pre"

(* HTML anchors and hyperlinks *)
fun ahref s     = elma "a" [("href",s)]
fun ahrefa a s  = elma "a" (("href",s)::a)
fun aname s     = elma "a" [("name",s)]

(* HTML text formats and style *)
val em       = elm "em"
val strong   = elm "strong"
val tt       = elm "tt"
val sub      = elm "sub"
val sup      = elm "sup"
                                 
(* HTML lists *)
val ul       = elm "ul"
val ula      = elma "ul"
val ol       = elm "ol"
val ola      = elma "ol"
val li       = elm "li"
val dl       = elm "dl"
val dla      = elma "dl"
val dt       = elm "dt"
val dd       = elm "dd"

(* HTML tables *)
val table    = elm "table"
val tablea   = elma "table"
val tr       = elm "tr"
val tra      = elma "tr"
val td       = elm "td"
val tda      = elma "td"
val th       = elm "th"
val tha      = elma "th"
val caption  = elm "caption"
val captiona = elma "caption"

(* HTML images *)
fun img s    = elma0 "img" [("src",s)]
fun imga a s = elma0 "img" (("src",s)::a)
                                                              
(* HTML forms etc *)
fun form s     = elma "form" [("action",s)]
fun forma a s  = elma "form" (("action",s)::a)
fun input t    = elma0 "input" [("type",t)]
fun inputa t a = elma0 "input" (("type",t)::a)
fun intext n a = inputa "text" (("name",n)::a)
fun inpassword n a = inputa "password" (("name",n)::a)
fun incheckbox {name : string, value : string} a = inputa "checkbox" (("name",name)::("value",value)::a)
fun inradio {name : string, value : string} a = inputa "radio" (("name",name)::("value",value)::a)
fun inreset v a = inputa "reset" (("value",v)::a) 
fun insubmit v a = inputa "submit" (("value",v)::a)
fun inhidden {name : string, value : string} = inputa "hidden" [("name",name),("value",value)]
fun textarea n = elma "textarea" [("name",n)]
fun textareaa n a = elma "textarea" (("name",n)::a)
fun select n a = elma "select" (("name",n)::a)
fun option v = elma0 "option" [("value",v)]
                               
(* HTML encoding  *)
fun urlencode s : string =
    let fun encode #" " = "+"
	  | encode #"-" = "-"
	  | encode #"_" = "_"
	  | encode #"." = "."
	  | encode c    = 
	    if Char.isAlphaNum c then String.str c 
	    else "%" ^ StringCvt.padLeft #"0" 2 
                       (Int.fmt StringCvt.HEX (Char.ord c))
    in String.translate encode s 
    end

fun htmlencode s : string =
    let fun encode #"<" = "&lt;"
	  | encode #">" = "&gt;"
	  | encode #"&" = "&amp;"
	  | encode #"\"" = "&#34;"
	  | encode c    = String.str c
    in String.translate encode s 
    end

end
