
local
 val a = 5

 val _ = ceil 1E23 handle _ => 34

 fun f b = a + b
in
 val _ = print (Int.toString (f 8))
end