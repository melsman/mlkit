(* Records can be allocated in explicit regions *)
fun f () : int =
    let with r
        val x = {a=3,b=5}`r
    in #b x
    end
