(* Refs can be allocated in explicit regions *)

infix +
fun !(x: 'a ref): 'a = prim ("!", x)

fun f () : int =
    let region r
        val x = ref`[r] 8
        val y = ref`r 9
    in !x + !y
    end
