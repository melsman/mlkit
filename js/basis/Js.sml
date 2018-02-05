structure JsSecret :>
  sig
    include JS
    val fromDoc : doc -> foreignptr
  end =
struct

structure J = JsCore

(* dom *)
type win = foreignptr
type doc = foreignptr
fun fromDoc a = a

type elem = foreignptr

type ns = string (* name space *)
val nsFromString = fn x => x

val document = J.exec0 {stmt="return document;", res=J.fptr} ()
val window = J.exec0 {stmt="return this;", res=J.fptr} ()

fun openWindow (url:string) (title:string) (attrs:string) : win =
    J.exec3 {stmt="return window.open(u,t,a);",
             arg1=("u",J.string),
             arg2=("t",J.string),
             arg3=("a",J.string),
             res=J.fptr} (url,title,attrs)

fun windowDocument w : doc =
    J.exec1 {stmt="return w.document;",
             arg1=("w",J.fptr),
             res=J.fptr} w

fun closeWindow w : unit =
    J.exec1 {stmt="w.close();",
             arg1=("w",J.fptr),
             res=J.unit} w

fun documentWrite d s =
    J.exec2 {stmt="return d.write(s);",
             arg1=("d",J.fptr),
             arg2=("s",J.string),
             res=J.unit} (d,s)

fun getElementById (d:doc) (id:string) : elem option =
    J.exec2 {stmt="return SmlPrims.option(d.getElementById(id));",
             arg1=("d",J.fptr),
             arg2=("id",J.string),
             res=J.option J.fptr} (d,id)

fun parent (e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.parentNode);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun firstChild (e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.firstChild);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun lastChild (e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.lastChild);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun nextSibling (e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.nextSibling);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun previousSibling (e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.previousSibling);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun innerHTML (e:elem) (s:string) : unit =
    J.exec2 {stmt="e.innerHTML = s;",
             arg1=("e",J.fptr),
             arg2=("s",J.string),
             res=J.unit} (e,s)

(* events *)
datatype eventType = onclick | onchange | onkeypress | onkeyup | onmouseover | onmouseout

fun installEventHandler (e: elem) (et: eventType) (f: unit -> bool) : unit =
    let val arg1 = ("e", J.fptr)
        val arg2 = ("f", J.==>(J.unit,J.bool))
        val stmt =
            case et of
              onclick     => "e.onclick = f;"
            | onchange    => "e.onchange = f;"
            | onkeypress  => "e.onkeypress = f;"
            | onkeyup     => "e.onkeyup = f;"
            | onmouseover => "e.onmouseover = f;"
            | onmouseout  => "e.onmouseout = f;"
    in
      J.exec2 {stmt=stmt, arg1=arg1,
               arg2=arg2, res=J.unit} (e,f)
    end

fun getEventHandler (e: elem) (et: eventType) : (unit -> bool) option =
    NONE

val IE = J.exec0 {stmt="return document.all?true:false;",
                  res=J.bool} ()

fun posF (f:int*int -> 'a) : int*int -> 'a =
    let fun pos a = if a < 0 then 0 else a
    in fn (x,y) => f (pos x, pos y)
    end

fun onMouseMoveIE (d:doc) (f : int*int->unit) : unit =
    J.exec2 {stmt="return d.onmousemove = function(e) { f([event.clientX + d.body.scrollLeft,event.clientY + d.body.scrollTop]); };",
             arg1=("d",J.fptr), arg2=("f",J.===>(J.int,J.int,J.unit)), res=J.unit} (d,posF f)


fun onMouseMoveNIE (d:doc) (f : int*int->unit) : unit =
    J.exec2 {stmt="return d.onmousemove = function(e) { f([e.pageX,e.pageY]); };",
             arg1=("d",J.fptr), arg2=("f",J.===>(J.int,J.int,J.unit)), res=J.unit} (d,posF f)

val onMouseMove =
    if IE then onMouseMoveIE
    else onMouseMoveNIE

fun xElem (e:elem) : int =
    J.exec1 {stmt="var x=0; \
                  \while(e) {x+=(e.offsetLeft-e.scrollLeft+e.clientLeft); \
                            \e=e.offsetParent; }; \
                  \return x;",
             arg1=("e",J.fptr), res=J.int} e

fun yElem (e:elem) : int =
    J.exec1 {stmt="var y=0; \
                  \while(e) {y+=(e.offsetTop-e.scrollTop+e.clientTop); \
                            \e=e.offsetParent; }; \
                  \return y;",
             arg1=("e",J.fptr), res=J.int} e

fun posFe (e:elem, f:int*int -> 'a) : int*int -> 'a =
    let val xOff = xElem e
        val yOff = yElem e
        fun pos a = if a < 0 then 0 else a
    in fn (x,y) => f (pos (x-xOff), pos (y-yOff))
    end

fun onMouseMoveElemIE (e:elem) (f : int*int->unit) : unit =
    J.exec2 {stmt="return e.ownerDocument.onmousemove = function(ee) { f([event.clientX + e.ownerDocument.body.scrollLeft, event.clientY + e.ownerDocument.body.scrollTop]); };",
             arg1=("e",J.fptr), arg2=("f",J.===>(J.int,J.int,J.unit)), res=J.unit} (e,posFe(e,f))


fun onMouseMoveElemNIE (e:elem) (f : int*int->unit) : unit =
    J.exec2 {stmt="return e.ownerDocument.onmousemove = function(ee) { f([ee.pageX,ee.pageY]); };",
             arg1=("e",J.fptr), arg2=("f",J.===>(J.int,J.int,J.unit)), res=J.unit} (e,posFe(e,f))

val onMouseMoveElem =
    if IE then onMouseMoveElemIE
    else onMouseMoveElemNIE

type intervalId = foreignptr
fun setInterval (i: int) (f: unit -> unit) : intervalId =
    let val arg1 = ("i", J.int)
        val arg2 = ("f", J.==>(J.unit,J.unit))
    in
      J.exec2 {stmt="return setInterval(f,i);",
               arg1=arg1, arg2=arg2,
               res=J.fptr} (i,f)
    end

fun clearInterval (id: intervalId) : unit =
    let val arg1 = ("id", J.fptr)
    in
      J.exec1 {stmt="return clearInterval(id);",
               arg1=arg1, res=J.unit} id
    end

type timeoutId = foreignptr
fun setTimeout (i: int) (f: unit -> unit) : timeoutId =
    let val arg1 = ("i", J.int)
        val arg2 = ("f", J.==>(J.unit,J.unit))
    in
      J.exec2 {stmt="return setTimeout(f,i);",
               arg1=arg1, arg2=arg2,
               res=J.fptr} (i,f)
    end

fun clearTimeout (id: timeoutId) : unit =
    let val arg1 = ("id", J.fptr)
    in
      J.exec1 {stmt="return clearTimeout(id);",
               arg1=arg1, res=J.unit} id
    end

fun value (e:elem) : string =
    J.exec1{stmt="return e.value;",
            arg1=("e",J.fptr),
            res=J.string} e

fun documentElement (d:doc) : elem =
    J.exec1{stmt="return d.documentElement;",
            arg1=("d",J.fptr),
            res=J.fptr} d

fun setCookie (d:doc) (v:string) : unit =
    JsCore.exec2{arg1=("document",JsCore.fptr),arg2=("value",JsCore.string),res=JsCore.unit,stmt="document.cookie=value;"} (d, v)

fun getCookie (d:doc) : string =
    JsCore.exec1{arg1=("document",JsCore.fptr),res=JsCore.string,stmt="return document.cookie;"} d

fun setStyle (e : elem) (a: string) : unit =
    J.exec2 {stmt="e.setAttribute('style',a); e.style.cssText = a;",
             arg1=("e",J.fptr), arg2=("a",J.string), res=J.unit} (e,a)

fun setAttribute (e : elem) (a: string) (b:string) : unit =
      J.exec3 {stmt="return e.setAttribute(a,b);",
               arg1=("e",J.fptr), arg2=("a",J.string), arg3=("b",J.string), res=J.unit} (e,a,b)

fun setAttributeNS (ns:ns) (e : elem) (a: string) (b:string) : unit =
      J.exec4 {stmt="return e.setAttributeNS(ns,a,b);",
               arg1=("ns",J.string),arg2=("e",J.fptr),
               arg3=("a",J.string), arg4=("b",J.string), res=J.unit} (ns,e,a,b)

fun removeAttribute (e : elem) (a: string) : unit =
    J.exec2 {stmt="return e.removeAttribute(a);",
             arg1=("e",J.fptr), arg2=("a",J.string), res=J.unit} (e,a)

fun createElement (t : string) : elem =
    J.call1 ("document.createElement", J.string, J.fptr) t

fun createElementNS (ns:ns) (t:string) : elem =
    J.call2 ("document.createElementNS", J.string, J.string, J.fptr) (ns,t)

fun createFragment () : elem =
    J.call0 ("document.createDocumentFragment", J.fptr)

fun createTextNode (t : string) : elem =
    J.call1 ("document.createTextNode", J.string, J.fptr) t

fun appendChild (e : elem) (a: elem) : unit =
    J.exec2 {stmt="return e.appendChild(a);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), res=J.unit} (e,a)

fun removeChild (e : elem) (a: elem) : unit =
    J.exec2 {stmt="return e.removeChild(a);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), res=J.unit} (e,a)

fun replaceChild (e : elem) (a: elem) (old: elem) : unit =
    J.exec3 {stmt="return e.replaceChild(a,old);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), arg3=("old",J.fptr),
             res=J.unit} (e,a,old)

fun setStyle (e: elem) (s:string,v:string) : unit =
    let val st = J.getProperty e J.fptr "style"
    in J.setProperty st J.string s v
    end

(* Shorthand notation for creating elements *)
structure Element = struct

  fun taga' createElem t attrs elem =
      let val newelem = createElem t
      in List.app (fn (k,v) => setAttribute newelem k v) attrs;
         appendChild newelem elem;
         newelem
      end

  val taga = taga' createElement
  fun nstaga ns = taga' (createElementNS ns)

  fun tag t = taga t nil
  fun nstag ns t = nstaga ns t nil

  fun taga0' createElem t attrs =
      let val newelem = createElem t
      in List.app (fn (k,v) => setAttribute newelem k v) attrs;
         newelem
      end
  val taga0 = taga0' createElement
  fun nstaga0 ns = taga0' (createElementNS ns)

  fun tag0 t = taga0 t nil
  fun nstag0 ns t = nstaga0 ns t nil

  fun $ s = createTextNode s

  infix &
  fun e1 & e2 =
      let val e = createFragment()
      in appendChild e e1;
         appendChild e e2;
         e
      end

  fun toForeignPtr x = x
  fun fromForeignPtr x = x
end

structure XMLHttpRequest =
  struct
      type req = foreignptr
      fun new () : req =
          J.call0 ("SmlPrims.newRequest", J.fptr)

      fun openn (r:req) {method: string, url: string, async: bool} : unit =
          J.exec4 {stmt="return r.open(m,u,a);",
                   arg1=("r",J.fptr),arg2=("m",J.string),arg3=("u",J.string),
                   arg4=("a",J.bool),res=J.unit} (r,method,url,async)

      fun send (r:req) (SOME s) : unit =
          J.exec2 {stmt="return r.send(s);",
                   arg1=("r",J.fptr),arg2=("s",J.string),res=J.unit} (r,s)
        | send r NONE =
          J.exec1 {stmt="return r.send(null);",
                   arg1=("r",J.fptr),res=J.unit} r

      fun sendBlob (r:req) (s:string) : unit =
          J.exec2 {stmt="return r.send(new Uint8Array(SmlPrims.charsToCharArray(SmlPrims.explode(s))));",
                   arg1=("r",J.fptr),arg2=("s", J.string),res=J.unit} (r, s)

      fun setRequestHeader (r:req) (k:string,v:string) : unit =
          J.exec3 {stmt="return r.setRequestHeader(k,v);",
                   arg1=("r",J.fptr),arg2=("k",J.string),arg3=("v",J.string),
                   res=J.unit} (r,k,v)

      fun setResponseType (r:req) (v:string) : unit =
          J.exec2 {stmt="r.responseType = v;",
                   arg1=("r",J.fptr),arg2=("v",J.string),
                   res=J.unit} (r,v)

      fun state (r:req) : int =
          J.exec1 {stmt="return r.readyState;",
                   arg1=("r",J.fptr),res=J.int} r

      fun status (r:req) : int option =
          J.exec1 {stmt="return SmlPrims.option(r.status);",
                   arg1=("r",J.fptr),
                   res=J.option J.int} r

      fun onStateChange (r:req) (f: unit -> unit) : unit =
          J.exec2{stmt="r.onreadystatechange = f;",
                  arg1=("r",J.fptr),arg2=("f",J.==>(J.unit,J.unit)),
                  res=J.unit} (r,f)

      fun response (r:req) : string option =
          J.exec1 {stmt="return SmlPrims.option(r.responseText);",
                   arg1=("r",J.fptr),
                   res=J.option J.string} r

      fun responseArrBuf (r:req) : string option =
          J.exec1 {stmt="return SmlPrims.option(SmlPrims.arraybufferToString(r.response));",
                   arg1=("r",J.fptr),
                   res=J.option J.string} r

      fun abort (r:req) : unit =
          J.exec1 {stmt="return r.abort();",
                   arg1=("r",J.fptr),
                   res=J.unit} r
  end

fun random () : real =
    J.exec0 {stmt="return Math.random();",
             res=J.real} ()

val unit2unit_T = J.==>(J.unit,J.unit)

fun loadScript url callback =
    let val head = J.exec1 {stmt="return document.getElementsByTagName(s)[0];",
                            arg1=("s", J.string),
                            res=J.fptr} "head"
        val head = Element.fromForeignPtr head
        val script = createElement "script"
        fun setScriptProp t k v =
            J.setProperty (Element.toForeignPtr script) t k v
        val () = setScriptProp J.string "type" "text/javascript"
        val () = setScriptProp J.string "src" url
        (* set callback on multiple properties for browser compatibility *)
        val () = setScriptProp unit2unit_T "onreadystatechange" callback
        val () = setScriptProp unit2unit_T "onload" callback
    in appendChild head script
    end
end

structure Js : JS = JsSecret
