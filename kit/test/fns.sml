(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  

val _ = print "Testing function applications, etc...\n"

fun f(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,
      a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
      a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,
      a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,
      a40,a41,a42,a43,a44,a45,a46,a47,a48,a49,
      a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,
      a60,a61,a62,a63,a64,a65,a66,a67,a68,a69,
      a70,a71,a72,a73,a74,a75,a76,a77,a78,a79,
      a80,a81,a82,a83,a84,a85,a86,a87,a88,a89,
      a90,a91,a92,a93,a94,a95,a96,a97,a98,a99)
    : int =
    a0+a1+a2+a3+a4+a5+a6+a7+a8+a9+
    a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+
    a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+
    a30+a31+a32+a33+a34+a35+a36+a37+a38+a39+
    a40+a41+a42+a43+a44+a45+a46+a47+a48+a49+
    a50+a51+a52+a53+a54+a55+a56+a57+a58+a59+
    a60+a61+a62+a63+a64+a65+a66+a67+a68+a69+
    a70+a71+a72+a73+a74+a75+a76+a77+a78+a79+
    a80+a81+a82+a83+a84+a85+a86+a87+a88+a89+
    a90+a91+a92+a93+a94+a95+a96+a97+a98+a99

val r1 = 
    f(0,1,2,3,4,5,6,7,8,9,
      10,11,12,13,14,15,16,17,18,19,
      20,21,22,23,24,25,26,27,28,29,
      30,31,32,33,34,35,36,37,38,39,
      40,41,42,43,44,45,46,47,48,49,
      50,51,52,53,54,55,56,57,58,59,
      60,61,62,63,64,65,66,67,68,69,
      70,71,72,73,74,75,76,77,78,79,
      80,81,82,83,84,85,86,87,88,89,
      90,91,92,93,94,95,96,97,98,99)

val _ = tst "test1" (r1 = (100*99) div 2)

fun g(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,
      a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
      a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,
      a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,
      a40,a41,a42,a43,a44,a45,a46,a47,a48,a49,
      a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,
      a60,a61,a62,a63,a64,a65,a66,a67,a68,a69,
      a70,a71,a72,a73,a74,a75,a76,a77,a78,a79,
      a80,a81,a82,a83,a84,a85,a86,a87,a88,a89,
      a90,a91,a92,a93,a94,a95,a96,a97,a98,a99)
    : real =
    a0+a1+a2+a3+a4+a5+a6+a7+a8+a9+
    a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+
    a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+
    a30+a31+a32+a33+a34+a35+a36+a37+a38+a39+
    a40+a41+a42+a43+a44+a45+a46+a47+a48+a49+
    a50+a51+a52+a53+a54+a55+a56+a57+a58+a59+
    a60+a61+a62+a63+a64+a65+a66+a67+a68+a69+
    a70+a71+a72+a73+a74+a75+a76+a77+a78+a79+
    a80+a81+a82+a83+a84+a85+a86+a87+a88+a89+
    a90+a91+a92+a93+a94+a95+a96+a97+a98+a99

val r2 =
    g(real 0,real 1,real 2,real 3,real 4,real 5,real 6,real 7,real 8,real 9,
      real 10,real 11,real 12,real 13,real 14,real 15,real 16,real 17,real 18,real 19,
      real 20,real 21,real 22,real 23,real 24,real 25,real 26,real 27,real 28,real 29,
      real 30,real 31,real 32,real 33,real 34,real 35,real 36,real 37,real 38,real 39,
      real 40,real 41,real 42,real 43,real 44,real 45,real 46,real 47,real 48,real 49,
      real 50,real 51,real 52,real 53,real 54,real 55,real 56,real 57,real 58,real 59,
      real 60,real 61,real 62,real 63,real 64,real 65,real 66,real 67,real 68,real 69,
      real 70,real 71,real 72,real 73,real 74,real 75,real 76,real 77,real 78,real 79,
      real 80,real 81,real 82,real 83,real 84,real 85,real 86,real 87,real 88,real 89,
      real 90,real 91,real 92,real 93,real 94,real 95,real 96,real 97,real 98,real 99)

val _ = tst "test2" (round r2 = (100*99) div 2)
    
fun mkI i : int = i*2

fun h () =
    let val a0 = mkI 0
	val a1 = mkI 1
	val a2 = mkI 2
	val a3 = mkI 3
	val a4 = mkI 4
	val a5 = mkI 5
	val a6 = mkI 6
	val a7 = mkI 7
	val a8 = mkI 8
	val a9 = mkI 9
	val a10 = mkI 10
	val a11 = mkI 11
	val a12 = mkI 12
	val a13 = mkI 13
	val a14 = mkI 14
	val a15 = mkI 15
	val a16 = mkI 16
	val a17 = mkI 17
	val a18 = mkI 18
	val a19 = mkI 19
	val a20 = mkI 20
	val a21 = mkI 21
	val a22 = mkI 22
	val a23 = mkI 23
	val a24 = mkI 24
	val a25 = mkI 25
	val a26 = mkI 26
	val a27 = mkI 27
	val a28 = mkI 28
	val a29 = mkI 29
	val a30 = mkI 30
	val a31 = mkI 31
	val a32 = mkI 32
	val a33 = mkI 33
	val a34 = mkI 34
	val a35 = mkI 35
	val a36 = mkI 36
	val a37 = mkI 37
	val a38 = mkI 38
	val a39 = mkI 39
	val a40 = mkI 40
	val a41 = mkI 41
	val a42 = mkI 42
	val a43 = mkI 43
	val a44 = mkI 44
	val a45 = mkI 45
	val a46 = mkI 46
	val a47 = mkI 47
	val a48 = mkI 48
	val a49 = mkI 49
	val a50 = mkI 50
	val a51 = mkI 51
	val a52 = mkI 52
	val a53 = mkI 53
	val a54 = mkI 54
	val a55 = mkI 55
	val a56 = mkI 56
	val a57 = mkI 57
	val a58 = mkI 58
	val a59 = mkI 59
	val a60 = mkI 60
	val a61 = mkI 61
	val a62 = mkI 62
	val a63 = mkI 63
	val a64 = mkI 64
	val a65 = mkI 65
	val a66 = mkI 66
	val a67 = mkI 67
	val a68 = mkI 68
	val a69 = mkI 69
	val a70 = mkI 70
	val a71 = mkI 71
	val a72 = mkI 72
	val a73 = mkI 73
	val a74 = mkI 74
	val a75 = mkI 75
	val a76 = mkI 76
	val a77 = mkI 77
	val a78 = mkI 78
	val a79 = mkI 79
	val a80 = mkI 80
	val a81 = mkI 81
	val a82 = mkI 82
	val a83 = mkI 83
	val a84 = mkI 84
	val a85 = mkI 85
	val a86 = mkI 86
	val a87 = mkI 87
	val a88 = mkI 88
	val a89 = mkI 89
	val a90 = mkI 90
	val a91 = mkI 91
	val a92 = mkI 92
	val a93 = mkI 93
	val a94 = mkI 94
	val a95 = mkI 95
	val a96 = mkI 96
	val a97 = mkI 97
	val a98 = mkI 98
	val a99 = mkI 99
	val f1 = f(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,
		   a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,
		   a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,
		   a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,
		   a40,a41,a42,a43,a44,a45,a46,a47,a48,a49,
		   a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,
		   a60,a61,a62,a63,a64,a65,a66,a67,a68,a69,
		   a70,a71,a72,a73,a74,a75,a76,a77,a78,a79,
		   a80,a81,a82,a83,a84,a85,a86,a87,a88,a89,
		   a90,a91,a92,a93,a94,a95,a96,a97,a98,a99)
	val g1 = g(real a0,real a1,real a2,real a3,real a4,real a5,real a6,real a7,real a8,real a9,real 
		   a10,real a11,real a12,real a13,real a14,real a15,real a16,real a17,real a18,real a19,real 
		   a20,real a21,real a22,real a23,real a24,real a25,real a26,real a27,real a28,real a29,real 
		   a30,real a31,real a32,real a33,real a34,real a35,real a36,real a37,real a38,real a39,real 
		   a40,real a41,real a42,real a43,real a44,real a45,real a46,real a47,real a48,real a49,real 
		   a50,real a51,real a52,real a53,real a54,real a55,real a56,real a57,real a58,real a59,real 
		   a60,real a61,real a62,real a63,real a64,real a65,real a66,real a67,real a68,real a69,real 
		   a70,real a71,real a72,real a73,real a74,real a75,real a76,real a77,real a78,real a79,real 
		   a80,real a81,real a82,real a83,real a84,real a85,real a86,real a87,real a88,real a89,real 
		   a90,real a91,real a92,real a93,real a94,real a95,real a96,real a97,real a98,real a99)
    in real f1 + g1
    end

val r3 = h()

val _ = tst "test3" (round r3 = 2*(100*99))
