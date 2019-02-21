val () = let
  infix + - <
  fun fib n = if n < 1 then 1 else fib (n-1) + fib (n-2)
in
  fib 37; ()
end
