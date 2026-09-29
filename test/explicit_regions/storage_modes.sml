(* Allocation and call annotations, including mixed region arguments. *)
infix 6 +
infix 4 >
fun print (s:string) : unit = prim("printStringML",s)
fun deref (x:'a ref) : 'a = prim("!",x)
fun direct () =
    let with rs rp rr
        val a = "A"`atbot rs
        val b = "B"`attop rs
        val p = (a,b)`atbot rp
        val r = ref`atbot rr p
        val (a,b) = deref r
    in print a; print b
    end
fun f `r () : real = 5.4`sat r
fun g `r () : real = f `sat r ()
fun pair `[r1 r2] () : real * real = (5.4`sat r1,6.4`sat r2)
fun mix `r () : real =
    let with r2
        val (x,y) = pair `[sat r, atbot r2] ()
    in x + y
    end
fun calls () =
    let with r r2
        val x = g `atbot r ()
        val y = f `attop r ()
        val (a,b) = pair `[attop r, atbot r2] ()
        val z = mix `attop r ()
    in if x + y + a + b + z > 30.0 then print "C" else print "FAIL"
    end
fun branches flag =
    let with r
        val s = if flag then "D"`attop r else "E"`attop r
    in print s
    end
datatype box = Box of string | Other of word
fun records () =
    let with rs rr rc
        val a = "E"`atbot rs
        val r = {text=a,flag=true}`atbot rr
        val b = Box`atbot rc (#text r)
    in case b of Box s => print s | Other _ => print "FAIL"
    end
val _ = (direct (); calls (); branches true; records ())
