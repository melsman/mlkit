structure IntFinMapPT_Test : sig end =
  struct
    structure PP = PrettyPrint
    structure IFM = IntFinMap
    fun member [] e = false
      | member (x::xs) e = x=e orelse member xs e

    infix ===
    fun l1 === l2 =
      foldl (fn (x,b) => b andalso member l2 x) (length l1 = length l2) l1

    fun mk [] = []
      | mk (x::xs) = (x,Int.toString x)::mk xs

    fun mk' [] = []
      | mk' (x::xs) = (x,Int.toString x ^ "'")::mk' xs

    val l1 = mk [12,234,345,23,234,6,456,78,345,23,78,79,657,345,234,456,78,0,7,45,3,56,578,7,567,345,35,2,456,57,8,5]
    val l2 = mk' [23,43,4,456,456,23,4523,4,47,5,567,4356,345,34,79,78,53,5,5,6,47,567,56,7,46,345,34,5,36,47,57]

    val m11 = IFM.fromList(mk [12,456,79,78,56,6])
    val m11not = IFM.fromList(mk [12,79,78,310,56])

    val m1 = IFM.fromList l1
    val m2 = IFM.fromList l2

    val m3 = IFM.plus(m1,m2)

    fun test s true = print ("OK : " ^ s ^ "\n")
      | test s false = print ("ERROR : " ^ s ^ "\n")

    val test1 = test "test1" (IFM.list(m3) === IFM.list(IFM.fromList(l1@l2)))
      
    val test2 = test "test2" (IFM.lookup m1 6 = SOME "6")
    val test3 = test "test3" (IFM.lookup m1 9 = NONE)

    val test4 = test "test4" (IFM.lookup m3 4356 = SOME "4356'")

    val test5 = test "test5" (IFM.lookup m3 35 = SOME "35")

    fun restrict (x,y) = IFM.restrict(Int.toString,x,y)

    val m4 = restrict (m3, [6,345,23,34,657,47])

    val test6 = test "test6" (IFM.lookup m4 23 = SOME "23'")
    val test6 = test "test6" (IFM.lookup m4 657 = SOME "657")
    val test7 = test "test7" (IFM.lookup m4 35 = NONE)
    val test8 = test "test8" (IFM.lookup m4 78 = NONE)

    val test9 = test "test9" ((restrict (m1,[43]); false) handle IFM.Restrict _ => true)

    fun sum [] = 0
      | sum (x::xs) = x + sum xs
      
    fun remdubs ([],a:int list) = a
      | remdubs (x::xs,a) = remdubs(xs, if member a x then a else x::a)

    val test10 = test "test10" (sum (IFM.dom m1) = sum (remdubs (map #1 l1,[])))

    val test11 = test "test11" (IFM.lookup (IFM.add(2222,"2222''",m1)) 2222 = SOME "2222''")
    val test12 = test "test12" (IFM.lookup (IFM.add(234,"234''",m1)) 234 = SOME "234''")

    val test13 = test "test13" (IFM.remove (328,m1) = NONE)
    val test14 = test "test14" (IFM.lookup (valOf(IFM.remove (345,m1))) 345 = NONE)
    val test15 = test "test15" (IFM.lookup (valOf(IFM.remove (345,m1))) 456 = SOME "456")
    val test16 = test "test16" (IFM.enrich (op =) (m1,m11))
    val test17 = test "test17" (not (IFM.enrich (op =) (m1,m11not)))

    val st = IFM.layoutMap {start="{",finish="}", eq=" -> ", sep=", "} (PP.LEAF o Int.toString) PP.LEAF m3

    val _ = PP.outputTree(print, st, 100)
  end

