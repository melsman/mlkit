let
  infix ::
  fun printList (l:int list) : unit = prim("printList", "printList", l)

  fun revAcc [] ys = ys
    | revAcc (x::xs) ys = revAcc xs (x::ys)

  val l = [1,2]

  val _ = printList l

  val l2 = revAcc l []
    
  val _ = printList l2

in ()
end