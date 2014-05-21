
local
open TimeVal
infix *** &&& >>>

val _ = print ("<html></head></head><body><h1>Timer frp ex1</h1>" ^
               "The time is <span id='timer'>not initialized</span>" ^
               "<h1>Text Input</h1>" ^
               "<input id='a' value='0'/> + <input id='b' value='0'/> = <span id='c'>?</span>" ^
               "<h1>Mouse over test</h1>" ^
               "<span id='over'>Test this</span> -> <span id='overres'>?</span>" ^
               "<h1>Mouse Position</h1>" ^
               "<span id='mouse0'>?</span><br />" ^
               "<span id='mouse1'>?</span><br />" ^
               "<span id='mouse2'>?</span><br />" ^
               "<h1>Click Events</h1>" ^
               "<input id='buttonDown' type='button' value='<==' /> &nbsp;" ^
               "<span id='clicks'>?</span> &nbsp;" ^ 
               "<input id='buttonUp' type='button' value='==>' />" ^
               "</body></html>")

val timer_b = timer 100

val bt = arr (Date.toString o Date.fromTimeLocal)

val _ = insertDOM(bt timer_b, "timer")

val si_t : (string,int,B)arr = arr (Option.valOf o Int.fromString)

val form = pair(textField "a",textField "b")

val t = (si_t *** si_t) >>> (arr op +) >>> (arr Int.toString)

val _ = insertDOM(t form, "c")

val ob = mouseOver "over"

val t : (bool,string,B) arr = arr (fn true => "Mouse is over" | false => "Mouse is not over")

val _ = insertDOM(t ob, "overres")

val t : (int*int,string,B) arr = arr (fn (x,y) => "[" ^ Int.toString x ^ "," ^ Int.toString y ^ "]")

val bm = mouse()

val t10 : (int*int,int*int,B) arr = arr (fn (x,y) => (x div 10 * 10, y div 10 * 10))

val bm2 = (t10 >>> t) bm

val _ = insertDOM(t bm, "mouse0")

val _ = insertDOM(calm 400 bm2, "mouse1")

val _ = insertDOM(delay 400 bm2, "mouse2")

datatype action = Up | Down
val esU = click "buttonUp" Up
val esD = click "buttonDown" Down
val es = merge(esU,esD)
val bc : (int,E)t = fold (fn (Up,a) => a + 1
                           | (Down,a) => a - 1) 0 es

val bc' : (int,B)t = hold 0 bc
val _ = insertDOM(arr Int.toString bc', "clicks")


in
end
