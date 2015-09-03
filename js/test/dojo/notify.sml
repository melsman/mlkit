
structure Notify : sig
  val notifyAreaElem : Js.elem
  val notify_err     : string -> unit
  val notify_errE    : Js.elem -> string -> unit
  val notify_warn    : string -> unit
  val notify         : string -> unit
end = struct

open Js.Element infix &

val notifyAreaContent = taga "div" [("class","notify_area_content")] ($"welcome!")
val notifyAreaElem = taga "div" [("class","notify_area")] notifyAreaContent

val notifyTimeout = ref NONE
fun setAttr e opacity color =
    Js.setAttribute e "style" ("opacity:" ^ Real.toString opacity ^ "; background-color:" ^ color ^ ";")
fun clearNotification e =
    (Js.setAttribute e "style" "opacity:0.0;";
     notifyTimeout := NONE)
fun fadeNotification color e opacity () =
    if opacity < 0.0 then clearNotification e
    else 
      (setAttr e opacity color;
       notifyTimeout := SOME(Js.setTimeout 50 (fadeNotification color e (opacity-0.02))))
          
val Error = "#dd8888"
val Warning = "#dddd88"
val Confirm = "#88dd88"
                  
fun notify0 color e s =
    (case !notifyTimeout of
         SOME tid => Js.clearTimeout tid
       | NONE => ();
     Js.innerHTML e s;
     setAttr e 1.0 color;
     notifyTimeout := SOME(Js.setTimeout 3000 (fadeNotification color e 0.98)))
        
val notify_err = notify0 Error notifyAreaContent
val notify_errE = notify0 Error
val notify_warn = notify0 Warning notifyAreaContent
val notify = notify0 Confirm notifyAreaContent

end
