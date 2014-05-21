let
  fun print (s:string):unit = prim ("printStringML",s)
  fun print_num (n:int):unit = prim ("printNum",n)

  infix ::
  infix +

  fun foldl f b xs = 
    case xs of 
      [] => b
    | x::xs' => foldl f (f x b) xs'


  fun foldl_uc (f,b,xs) =
    case xs of
      [] => b
    | x::xs' => foldl_uc (f,f(x,b),xs')

  val l = [1,2,3,4,5,6,7,8,9,10]

  val _ = print "Before fold\n"
  val n = foldl_uc (op +,0,l)
  val _ = print "Efter fold\n"

  val _ = print_num n


  val n = foldl (fn i => fn b => i+b) 0 l
  val _ = print_num n


  val f = (fn x => print x,25)
    
  val _ = #1(f) "hej\n"

in
  ()
end

