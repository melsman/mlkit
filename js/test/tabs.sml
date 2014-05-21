
open Js.Element infix &

fun nbsp() = $ ""

fun tabs tabwidth (spec:(string*Js.elem) list) =
    let
      fun assoc k nil = NONE
        | assoc k ((k0,v)::rest) = if k = k0 then SOME v else assoc k rest
        infix &
        val selection = ref (#1(List.hd spec))
        val links : (string * Js.elem) list ref = ref[]
        val table = Js.createElement "table"
        val _ = Js.setAttribute table "border" "0"
        val _ = Js.setAttribute table "spacing" "0"
        val _ = Js.setAttribute table "padding" "0"
        val _ = Js.setAttribute table "width" "100%"
        val divelem = tag "div" (#2(List.hd spec))
        fun unchooselink h =
            case assoc h (!links) of
              SOME e => Js.setAttribute e "bgcolor" "lightgrey"
            | NONE => ()
        fun chooselink h =
            case assoc h (!links) of
              SOME e => (Js.removeAttribute e "bgcolor")
            | NONE => ()
        fun remove h =
            case Js.firstChild divelem of
              SOME e => (Js.removeChild divelem e; unchooselink h)                         
            | NONE => raise Fail "tabs.remove"
        fun insert h e = (Js.appendChild divelem e; chooselink h)
        fun click h e () = (remove (!selection); insert h e; selection := h; true)
        fun link h e =
            let val th = taga "th" [("width",tabwidth)] ($ h)
            in Js.installEventHandler th Js.onclick (click h e);
               (if !selection <> h then Js.setAttribute th "bgcolor" "lightgrey"
                else ());
               th
            end
        val columns = List.length spec + 1
        val headingrow = Js.createElement "tr"
        val _ = Js.setAttribute headingrow "width" "100%"
        val _ = links := List.map (fn (h,e) => let val l = link h e 
                                               in Js.appendChild headingrow l;
                                                  (h,l)
                                               end) spec
        val th = Js.createElement "th"                                  
        val _ = Js.appendChild th (nbsp())
        val _ = Js.appendChild headingrow th
        val bodyrow = Js.createElement "tr"
        val td = Js.createElement "td"
        val _ = Js.setAttribute td "colspan" (Int.toString columns)
        val _ = Js.appendChild bodyrow td
        val _ = Js.appendChild td divelem
        val _ = Js.setAttribute headingrow "width" "100%"
        
    in Js.appendChild table headingrow;
       Js.appendChild table bodyrow;
       table
    end

val page1 = $ "Hello World."
val page2 = $ "God dag."
val page3 = tag "b" ($"What a day." & tag "p" ($"Ok"))

val table = tabs "70px" [("Tab1", page1), ("Tab2", page2), ("Tab3",page3)]

val table = taga "table" [("width","100%"),("border","1")] (tag "tr" (tag "td" table))

val win = Js.openWindow "" "" "newwin"

val doc = Js.windowDocument win

val top = Js.documentElement doc

val styles = 
    "body { font-size: 80%; font-family: 'Lucida Grande', Verdana, Arial, Sans-Serif; }\n\
    \ul#tabs { list-style-type: none; margin: 30px 0 0 0; padding: 0 0 0.3em 0; }\n\
    \ul#tabs li { display: inline; }\n\
    \ul#tabs li a { color: #42454a; background-color: #dedbde; border: 1px solid #c9c3ba; border-bottom: none; padding: 0.3em; text-decoration: none; }\n\
    \ul#tabs li a:hover { background-color: #f1f0ee; }\n\
    \ul#tabs li a.selected { color: #000; background-color: #f1f0ee; font-weight: bold; padding: 0.7em 0.3em 0.38em 0.3em; }\n\
    \div.tabContent { border: 1px solid #c9c3ba; padding: 0.5em; background-color: #f1f0ee; }\n\
    \div.tabContent.hide { display: none; }"

val h = tag "html" 
            (tag "head" (tag "title" ($"Tab Example") & (taga "style" [("type","text/css")] ($styles))) &
                 (tag "body" (tag "h1" ($"Tabs Example") & table & tag "p" ($"Hello"))))

val _ = Js.appendChild top h
