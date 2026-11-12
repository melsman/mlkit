(* Exercise runtime string equality independently of the reference byte loop. *)
fun equal (a:string, b:string) = a = b
fun reference (a, b) =
    let val n = String.size a
        fun loop i = i = n orelse
            (String.sub(a,i) = String.sub(b,i) andalso loop(i+1))
    in n = String.size b andalso loop 0
    end
fun check true = ()
  | check false = raise Fail "native string equality"
fun make n = String.implode (List.tabulate(n, fn i => Char.chr(i mod 256)))
val lengths = [0,1,2,7,8,9,15,16,17,31,32,33,255,256,257,4096]
val strings = List.map make lengths
val () = List.app (fn a => List.app
    (fn b => check(equal(a,b) = reference(a,b))) strings) strings
val () = List.app (fn a =>
    let val copy = String.implode (String.explode a)
        val () = check(equal(a,a))
        val () = check(equal(a,copy))
        fun mismatch i =
            if i = String.size a then ()
            else
                let val changed = String.substring(a,0,i) ^
                        String.str(Char.chr((Char.ord(String.sub(a,i))+1) mod 256)) ^
                        String.extract(a,i+1,NONE)
                in check(not(equal(a,changed))); mismatch(i+1)
                end
    in mismatch 0
    end) strings
val () = check(not(equal("a\000b","a\000c")))
val () = print "OK\n"
