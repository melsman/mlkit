
fun mkBox e0 id border =
    let val e = Js.createElement "div"
      val () = Js.setAttribute e "id" id
      val () = Js.appendChild e e0
      val () = Js.setStyle e ("position", "absolute")
      val () = Js.setStyle e ("border", "solid black 3px")
    in Js.appendChild body e
    end

fun mkBox n : (int*int)b * ((int*int)b -> unit) =
      let val id = "sheep" ^ Int.toString n
          val e = Js.createElement "img"
          val () = Js.setAttribute e "src" "sheep.png"
          val () = mkBox e id false
          val sheepPos0 = (200 + 70 * n, 200 + 70 * n)
          val s = hold sheepPos0 (fold newSheep sheepPos0 (changes m))
      in (s, setPos id)
      end
