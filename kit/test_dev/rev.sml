let
  infix ::
  fun printNum (i:int) : unit = prim("printNum", "printNum", i)

  fun revAcc [] ys = ys
    | revAcc (x::xs) ys = revAcc xs (x::ys)

  fun pr_list [] = ()
    | pr_list (x::xs) = (printNum x; pr_list xs)

  val l = [1,2]
in pr_list (revAcc l [])
end