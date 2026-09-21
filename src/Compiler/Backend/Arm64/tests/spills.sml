infix 6 + -
fun spill__noinline(a0:word,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39) =
  let val ignored:word = prim("getchar",())
  in a0+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34+a35+a36+a37+a38+a39-ignored+0w1 end
val w:word = prim("getchar",())
val _:unit = prim("putchar",spill__noinline(w,w,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0))
val _:unit = prim("putchar",0w10)
