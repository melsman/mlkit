functor WordN(W : WORD) :> WORD =
  struct
    type word = W.word
    val wordSize = W.wordSize
    val toLarge = W.toLarge
    val high = W.<<(W.fromInt ~1, Word.fromInt W.wordSize)
    val low = W.xorb(W.fromInt ~1, high)
    fun signextend w = if W.>> (w, Word.fromInt (wordSize - 1)) = W.fromInt 0 then w else W.orb(high,w)
    fun norm w = W.andb(w,low)
    val toLargeX = W.toLargeX o signextend
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLarge = norm o W.fromLarge
    val fromLargeWord = fromLarge
    val toLargeInt = W.toLargeInt
    val toLargeIntX = W.toLargeIntX o signextend
    val fromLargeInt = norm o W.fromLargeInt
    val toInt = W.toInt
    val toIntX = W.toIntX o signextend
    val fromInt = norm o W.fromInt

    val andb = W.andb
    val orb = W.orb
    val xorb = W.xorb
    val notb = norm o W.notb
    val << = norm o W.<<
    val >> = W.>>
    val ~>> = norm o W.~>> o (fn (x,y) => (signextend x,y))

    val op+ = norm o W.+
    val op- = norm o W.-
    val op* = norm o W.*
    val op div = W.div
    val op mod = W.mod

    val compare = W.compare
    val op< = W.<
    val op<= = W.<=
    val op> = W.>
    val op>= = W.>=

    val ~ = norm o W.~
    val min = W.min
    val max = W.max

    val fmt = W.fmt
    val toString = W.toString
    fun scan r gc s = let val v = W.scan r gc s in Option.map (fn (w,r) => if W.andb(w,high) = W.fromInt 0 then (w,r) else raise Overflow) v end
    fun fromString s = let val v = W.fromString s in Option.map (fn w => if W.andb(w,high) = W.fromInt 0 then w else raise Overflow) v end
  end
(*
structure Word30 = WF(struct open Word val wordSize = 30 end)
structure Word29 = WF(struct open Word val wordSize = 29 end)
structure Word28 = WF(struct open Word val wordSize = 28 end)
structure Word27 = WF(struct open Word val wordSize = 27 end)
structure Word26 = WF(struct open Word val wordSize = 26 end)
structure Word25 = WF(struct open Word val wordSize = 25 end)
structure Word24 = WF(struct open Word val wordSize = 24 end)
structure Word23 = WF(struct open Word val wordSize = 23 end)
structure Word22 = WF(struct open Word val wordSize = 22 end)
structure Word21 = WF(struct open Word val wordSize = 21 end)
structure Word20 = WF(struct open Word val wordSize = 20 end)
structure Word19 = WF(struct open Word val wordSize = 19 end)
structure Word18 = WF(struct open Word val wordSize = 18 end)
structure Word17 = WF(struct open Word val wordSize = 17 end)
structure Word16 = WF(struct open Word val wordSize = 16 end)
structure Word15 = WF(struct open Word val wordSize = 15 end)
structure Word14 = WF(struct open Word val wordSize = 14 end)
structure Word13 = WF(struct open Word val wordSize = 13 end)
structure Word12 = WF(struct open Word val wordSize = 12 end)
structure Word11 = WF(struct open Word val wordSize = 11 end)
structure Word10 = WF(struct open Word val wordSize = 10 end)
structure Word9 = WF(struct open Word val wordSize = 9 end)
structure Word7 = WF(struct open Word8 val wordSize = 7 end)
structure Word6 = WF(struct open Word8 val wordSize = 6 end)
structure Word5 = WF(struct open Word8 val wordSize = 5 end)
structure Word4 = WF(struct open Word8 val wordSize = 4 end)
structure Word3 = WF(struct open Word8 val wordSize = 3 end)
structure Word2 = WF(struct open Word8 val wordSize = 2 end)
structure Word1 = WF(struct open Word8 val wordSize = 1 end)
*)
