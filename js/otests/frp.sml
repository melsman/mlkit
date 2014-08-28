(* File frp.sml: Basic functional reative programming demonstration.
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

open Js.Element Rwp
infix *** &&& >>> &

val timerE = tag "span" ($"not initialized yet")
val aE = taga0 "input" [("value","0")]
val bE = taga0 "input" [("value","0")]
val cE = tag "span" ($"?")
val overE = tag "span" ($"Test This")
val overresE = tag "span" ($"?")
val mouse0E = tag "span" ($"?")
val mouse1E = tag "span" ($"?")
val mouse2E = tag "span" ($"?")
val buttonUpE = taga0 "input" [("type","button"),("value","==>")]
val buttonDownE = taga0 "input" [("type","button"),("value","<==")]
val clicksE = tag "span" ($"?")

val body = 
    tag "h2" ($"Time") & $"The time is " & timerE &
    tag "h2" ($"Text Input") & 
    aE & $" + " & bE & $" = " & cE &
    tag "h2" ($"Mouse Over") & 
    overE & $" -> " & overresE &
    tag "h2" ($"Mouse Position") & 
    mouse0E & $" - position" & tag0 "br" &
    mouse1E & $" - calmed version of position (rounded off to nearest 10)" & tag0 "br" &
    mouse2E & $" - position delayed 400ms" & tag0 "br" &
    tag "h2" ($"Click Events") &
    buttonDownE & clicksE & buttonUpE

val () = Dojo.runDialog "Rwp Demonstration" body

val timer_b = timer 100
val bt = arr (Date.toString o Date.fromTimeLocal)

val _ = insertDOM_elem timerE (bt timer_b)

val si_t : (string,int,B)arr = arr (Option.valOf o Int.fromString)

val form = pair(textField_elem aE, textField_elem bE)

val t = (si_t *** si_t) >>> (arr op +) >>> (arr Int.toString)

val _ = insertDOM_elem cE (t form)

val ob = mouseOver_elem overE

val t : (bool,string,B) arr = arr (fn true => "Mouse is over" | false => "Mouse is not over")

val _ = insertDOM_elem overresE (t ob)

val t : (int*int,string,B) arr = arr (fn (x,y) => "[" ^ Int.toString x ^ "," ^ Int.toString y ^ "]")

val bm = mouse()

val t10 : (int*int,int*int,B) arr = arr (fn (x,y) => (x div 10 * 10, y div 10 * 10))

val bm2 = (t10 >>> t) bm

val _ = insertDOM_elem mouse0E (t bm)

val _ = insertDOM_elem mouse1E (calm 400 bm2)

val _ = insertDOM_elem mouse2E (delay 400 (t bm))

datatype action = Up | Down
val esU = click_elem buttonUpE Up
val esD = click_elem buttonDownE Down
val es = merge(esU,esD)
val bc : (int,E)t = fold (fn (Up,a) => a + 1
                           | (Down,a) => a - 1) 0 es

val bc' : (int,B)t = hold 0 bc
val _ = insertDOM_elem clicksE (arr Int.toString bc')
