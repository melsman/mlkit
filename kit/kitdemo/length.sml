
fun upto n = 
  let fun loop(p as (0,acc)) = p
        | loop(n, acc) = loop(n-1, n::acc)
  in
      #2(loop(n,[]))
  end

fun nlength [] = 0
  | nlength (_::xs) = 1 + nlength xs

fun tlength'([], acc) = acc
  | tlength'(_::xs, acc) = tlength'(xs,acc+1)

fun tlength(l) = tlength'(l,0)

fun klength l =
  let fun loop(p as ([], acc)) = p
        | loop(_::xs, acc) = loop(xs,acc+1)
  in
      #2(loop(l,0))
  end

local 
  fun llength'(p as ([], acc)) = p
    | llength'(_::xs, acc) = llength'(xs,acc+1)
in
  fun llength(l) = #2(llength'(l, 0))
end

fun global(p as ([], acc)) = p
  | global(_::xs, acc) = global(xs, acc+1)

fun glength(l) = #2(global(l, 0))

val run =   nlength(upto 10000) + tlength(upto 10000) +
            klength(upto 10000) + llength(upto 10000) +
            glength(upto 10000);

