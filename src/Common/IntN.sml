functor IntN(I : INTEGER) :> INTEGER =
  struct

    structure W = Word
    type int = I.int
    val precision = I.precision
    val wordSize = case precision of
                       SOME n => n
                     | NONE => raise Fail "IntN.wordSize not supported"
    val i2w = W.fromInt o I.toInt
    val w2i = I.fromInt o W.toIntX
    val high = W.<<(W.fromInt ~1, W.fromInt wordSize)
    val low = W.xorb(W.fromInt ~1, high)

    fun signextend w = if W.>> (w, W.fromInt (Int.-(wordSize,1))) = W.fromInt 0
                       then w else W.orb(high,w)

    val signextendi = w2i o signextend o i2w

    val minInt0 = w2i (W.<<(0w1,W.fromInt (Int.-(wordSize,1))))
    val minInt0I = signextendi minInt0
    val maxInt0 = w2i (W.>>(low,0w1))
    val minInt = SOME minInt0
    val maxInt = SOME maxInt0

    val toLarge = I.toLarge o signextendi

    fun chk i = if I.<=(minInt0I,i) andalso I.<=(i,maxInt0)
                then i else raise Overflow

    fun norm w = W.andb(w,low)
    val normi = w2i o norm o i2w o chk
    val toLarge = I.toLarge o signextendi
    val fromLarge = normi o I.fromLarge
    val toInt = I.toInt o signextendi
    val fromInt = normi o I.fromInt

    infix oo
    fun f oo g = fn (a,b) => f(g a,g b)

    val op + = normi o I.+ oo signextendi
    val op - = normi o I.- oo signextendi
    val op * = normi o I.* oo signextendi
    val op div = normi o I.div oo signextendi
    val op mod = normi o I.mod oo signextendi
    val op quot = normi o I.quot oo signextendi
    val op rem = normi o I.rem oo signextendi

    val compare = I.compare oo signextendi
    val op < = I.< oo signextendi
    val op <= = I.<= oo signextendi
    val op > = I.> oo signextendi
    val op >= = I.>= oo signextendi

    val ~ = normi o I.~ o signextendi
    val abs = normi o I.abs o signextendi
    val min = normi o I.min oo signextendi
    val max = normi o I.max oo signextendi
    val sign = I.sign o signextendi
    val sameSign = I.sameSign oo signextendi

    val fmt = fn r => I.fmt r o signextendi
    val toString = I.toString o signextendi

    fun scan r gc s =
        Option.map (fn (i,r) => (normi i,r))
                   (I.scan r gc s)

    val fromString = Option.map normi o I.fromString
  end
