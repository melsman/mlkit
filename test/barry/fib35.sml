(*kitfib35.sml*)

let
  fun fib n = if n < 1 then 1 else fib (n-1) + fib (n-2)
in
  print (Int.toString (fib 35) ^ "\n")
end
