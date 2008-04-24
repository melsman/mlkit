structure Color :> COLOR = struct
  type t = string

  fun fromHex (w: word) : t =
      let val s = Word.toString w
      in "#" ^ CharVector.tabulate(size s - 6, fn _ => "0") ^ s
      end

  fun rgb (r,g,b) =
      let val i = 256 * 256 * r + 256 * g + b
      in fromHex(Word.fromInt i)
      end

  fun fromString (s: string) = SOME s

  fun toString s = s

  val aqua    : t = "aqua"
  val black   : t = "black"
  val blue    : t = "blue"
  val fuchsia : t = "fuchsia"
  val gray    : t = "gray"
  val green   : t = "green"
  val lime    : t = "lime"
  val maroon  : t = "maroon"
  val navy    : t = "navy"
  val olive   : t = "olive"
  val purple  : t = "purple"
  val red     : t = "red"
  val silver  : t = "silver"
  val teal    : t = "teal"
  val white   : t = "white"
  val yellow  : t = "yellow"
end
