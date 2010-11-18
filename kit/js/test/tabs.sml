
fun taga (t:string) (attrs:(string*string)list) (elem:Js.elem) : Js.elem = 
    let val newelem = Js.createElement t     
    in List.app (fn (k,v) => Js.setAttribute newelem k v) attrs;
       Js.appendChild newelem elem;
       newelem
    end

fun tag t elem = taga t nil elem

fun $ s = Js.createTextNode s

infix &

fun e1 & e2 =
    let val e = Js.createFragment()
    in Js.appendChild e e1;
       Js.appendChild e e2;
       e
    end

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

val top = Js.documentElement(Js.document)

val h = tag "html" (tag "body" (tag "h1" ($"Tabs Example") & table & tag "p" ($"Hello")))

val _ = Js.appendChild top h
