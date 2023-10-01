(* It is an error to give multiple explicit regions to ref *)

infix +
fun !(x: 'a ref): 'a = prim ("!", x)

fun f () : int =
    let with r
        val x = ref`[r r] 8
        val y = ref`r 9
    in !x + !y
    end
