(* File svgballs.sml: SVG demonstration.
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

open Js.Element infix &

val ns = Js.nsFromString "http://www.w3.org/2000/svg"

fun mkCircle cy color =
  nstaga0 ns "circle" [("cx","50"),("cy",cy),("r","20"),
                       ("stroke","black"),("stroke-width","3"),
                       ("fill",color)]

val circle1 = mkCircle "50" "blue"
val circle2 = mkCircle "150" "green"
           
val () = Dojo.runDialog "Svg Example"
          (nstaga ns "svg" [("height","200"),("width","400")] 
           (circle1 & circle2))

open Rwp

val time0 = Time.now()
val time = arr (fn t => Time.-(t,time0)) (timer 25)
val ts = arr (fn t => IntInf.toInt(Time.toMilliseconds t mod 1000)) time

fun coord f w t =
	Int.toString(round(f(2.0 * Math.pi * (real t) / 1000.0) * w + 2.0 * w))

val () = setAttr_elem circle1 ("cx", arr (coord Math.cos 100.0) ts)
val () = setAttr_elem circle2 ("cx", arr (coord Math.sin 50.0) ts)
