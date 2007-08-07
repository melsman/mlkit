signature JS = 
sig
  (* dom *)
  type doc type elem
  val document : doc
  val getElementById : doc -> string -> elem option
  val innerHTML : elem -> string -> unit

  (* events *)
  datatype eventType = onclick | onchange
  val installEventHandler : elem -> eventType -> (unit -> bool) -> unit
  val getEventHandler : elem -> eventType -> (unit -> bool) option

  type intervalId
  val setInterval : int -> (unit -> unit) -> intervalId
  val clearInterval : intervalId -> unit

  type timeoutId
  val setTimeout : int -> (unit -> unit) -> timeoutId
  val clearTimeout : timeoutId -> unit

  val value : elem -> string
(*
    val setAttribute : elem -> string -> string -> unit
*)
end

structure Js :> JS =
struct

structure J = JsCore

(* dom *)
type doc = foreignptr
type elem = foreignptr

val document = J.exec0 {stmt="return document;", res=J.fptr} ()

fun getElementById (d:doc) (id:string) : elem option =
    J.exec2 {stmt="return SmlPrims.option(d.getElementById(id));",
             arg1=("d",J.fptr),
             arg2=("id",J.string),
             res=J.option J.fptr} (d,id)

fun innerHTML (e:elem) (s:string) : unit =
    J.exec2 {stmt="e.innerHTML = s;",
             arg1=("e",J.fptr),
             arg2=("s",J.string),
             res=J.unit} (e,s)

(* events *)
datatype eventType = onclick | onchange
                     
fun installEventHandler (e: elem) (et: eventType) (f: unit -> bool) : unit =
    let val arg1 = ("e", J.fptr)
        val arg2 = ("f", J.==>(J.unit,J.bool))
    in
      case et of
        onclick => J.exec2 {stmt="e.onclick = f;", arg1=arg1, 
                            arg2=arg2, res=J.unit} (e,f)
      | onchange => J.exec2 {stmt="e.onchange = f;", arg1=arg1, 
                             arg2=arg2, res=J.unit} (e,f)
    end
    
fun getEventHandler (e: elem) (et: eventType) : (unit -> bool) option =
    NONE


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
      J.exec1 {stmt="return d.clearTimeout(id);", 
               arg1=arg1, res=J.unit} id
    end

fun value (e:elem) : string =
    J.exec1{stmt="return e.value;",
            arg1=("e",J.fptr),
            res=J.string} e

end
