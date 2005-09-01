local
exception S of int
in
fun f 0 : unit = raise S 1
  | f a = f a

end