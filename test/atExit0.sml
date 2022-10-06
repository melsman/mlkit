
val get : unit -> int =
    let val c = ref 0
    in fn () => !c before c := (!c + 3)
    end

fun fib n =
    if n <= 1 then (print "."; 1) else fib (n-1) + fib (n-2)

fun f s () =
    let val n = get()
        val m = fib n
    in print ("hello " ^ s ^ " - fib(" ^ Int.toString n ^ ") = " ^ Int.toString m ^ "\n")
    end

val () = OS.Process.atExit (f "a")
val () = OS.Process.atExit (f "b")
val () = OS.Process.atExit (f "c")
val () = OS.Process.atExit (f "d")
