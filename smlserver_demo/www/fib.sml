
fun fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)

fun show i = 
 `<html>
   <body bgcolor=white>
    <h2>Fib(^(Int.toString i)) = ^(Int.toString (fib i))</h2>
    <p>Go back to the <a href="index.sml">index page</a>...
   </body>
  </html>`

val _ = Ns.return (show 36)



