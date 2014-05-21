structure Test = struct
open Dojo infix >>=

fun panes n =
    if n = 0 then ret []
    else pane [("title", "Tab" ^ Int.toString n),EditorIcon.newPage] >>= (fn p => 
         panes (n-1) >>= (fn ps =>
         ret (p::ps)))

fun getElem id : Js.elem =
    case Js.getElementById Js.document id of
        SOME e => e
      | NONE => raise Fail "getElem"

(* User Code *)

val treedata = [
[("id","0"),("name","World")],
[("id","1"),("name","Europe"),("parent","0")],
[("id","2"),("name","Africa"),("parent","0")],
[("id","3"),("name","North America"),("parent","0")],
[("id","4"),("name","Germany"),("parent","1")],
[("id","5"),("name","Denmark"),("parent","1")],
[("id","6"),("name","Italy"),("parent","1")],
[("id","7"),("name","Norway"),("parent","1")],
[("id","8"),("name","Asia"),("parent","0")],
[("id","9"),("name","USA"),("parent","3")],
[("id","10"),("name","Canada"),("parent","3")],
[("id","11"),("name","Mexico"),("parent","3")]
]

val () = print "<link rel='stylesheet' href='dijit/themes/claro/claro.css'>"
val () = print "<style>html, body, #mainDiv { width: 100%; height: 100%; border: 0; padding: 0; margin: 0;}</style>"
val () = print "<body class='claro' id='body'></body>"

fun onClick w id = setContent w ("Got " ^ id)

val m =
  pane [("region", "top"), ("content","<b>Main Title</b>")] >>= (fn toppane =>
  pane [("content","<b>Some stuff</b><br>a line<br>another line"),("style","height:100%;")] >>= (fn botpane =>
  titlePane [("title","Output"),("region", "bottom"),
         ("splitter","true"), ("style","border:0;")] botpane >>= (fn botpane =>
  linkPane [("region", "right"), ("href","doc/ARRAY.sml.html"), ("style", "width:20%;"),
            ("splitter","true")] >>= (fn rightpane =>
  pane [("title", "First"),("closable","true"),EditorIcon.unlink] >>= (fn p1 =>
  pane [("title", "Second"),EditorIcon.save] >>= (fn p2 =>
  treeStore treedata >>= (fn store =>
  tree [] "0" (onClick p1) store >>= (fn tr =>
  panes 4 >>= (fn ps =>
  titlePane [("title","A Tree"),("region","left"),("splitter","true"),
             ("style","width:10%;")] tr >>= (fn tr =>
  accordionContainer [("title","Something"),("region","right"),
                      ("splitter","true")] ps >>= (fn ac =>
  tabContainer [("region","center"),("tabPosition","bottom")] [p1,p2,ac] >>= (fn tc =>
  borderContainer [("region","center"),
                   ("style", "height: 100%; width: 100%;")] [tc,botpane] >>= (fn ibc =>
  borderContainer [("style", "height: 100%; width: 100%;")]
                  [tr,ibc,toppane,rightpane]
)))))))))))))

val () = attachToElement (getElem "body") m
end
