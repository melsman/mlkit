(*val _ = SMLofNJ.Internals.GC.messages false *)

(* test/pickletest.sml; Martin Elsman 2003-07-01 *)

local 
    infix 1 seq
    fun e1 seq e2 = e2;
    fun check b = if b then "OK" else "WRONG";
    fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"
	
    fun range (from, to) p = 
	let open Int 
	in (from > to) orelse (p from) andalso (range (from+1, to) p)
	end

    fun checkrange bounds = check o range bounds
	
    fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n")
    fun tst  s b = tst0 s (check  b)
    fun tst' s f = tst0 s (check' f)
	
    fun tstrange s bounds = (tst s) o range bounds  

    val _ = print "\nFile pickle.sml: Testing structure Pickle...\n"

    open Pickle

    fun tm_eq(t1,t2) =
	Time.<(Time.-(t1,t2),Time.fromSeconds 2)
	handle _ => Time.<(Time.-(t2,t1),Time.fromSeconds 2)

    fun okEq' eq s_tst (pu: 'a pu) (v1 : 'a) =
	tst' s_tst 
	(fn () => 
	 let val s = toString (pickler pu v1 (empty()))
	     val (v2,is) = unpickler pu (fromString s)
	 in eq(v1,v2)
	 end)

    fun okEq a = okEq' (op =) a

    val maxInt = 
	case Int.maxInt of 
	    SOME i => i
	  | _ => raise Fail "maxInt"

    val maxWord =
	0w2 * Word.fromInt maxInt + 0w1

    val minInt = 
	case Int.minInt of 
	    SOME i => i
	  | _ => raise Fail "minInt"

    val maxInt32 = 
	case Int32.maxInt of 
	    SOME i => i
	  | _ => raise Fail "maxInt32"

    val maxWord32 =
	0w2 * Word32.fromLargeInt maxInt32 + 0w1

    val minInt32 = 
	case Int32.minInt of 
	    SOME i => i
	  | _ => raise Fail "minInt32"
in	

(* Word *)
val _ = okEq "test1a" word 0w0
val _ = okEq "test1b" word 0w7
val _ = okEq "test1c" word (Word.fromInt maxInt)
val _ = okEq "test1d" word (Word.fromInt minInt)
val _ = okEq "test1e" word maxWord

(* Word32 *)
val _ = okEq "test2a" word32 0w0
val _ = okEq "test2b" word32 0w7
val _ = okEq "test2c" word32 (Word32.fromLargeInt maxInt32)
val _ = okEq "test2d" word32 (Word32.fromLargeInt minInt32)
val _ = okEq "test2e" word32 maxWord32

(* Int *)
val _ = okEq "test3a" int 0
val _ = okEq "test3b" int 7
val _ = okEq "test3c" int ~7
val _ = okEq "test3d" int maxInt
val _ = okEq "test3e" int minInt

(* Int32 *)
val _ = okEq "test4a" int32 0
val _ = okEq "test4b" int32 7
val _ = okEq "test4c" int32 ~7
val _ = okEq "test4d" int32 maxInt32
val _ = okEq "test4e" int32 minInt32

(* Bool *)
val _ = okEq "test5a" bool true
val _ = okEq "test5b" bool false

(* String *)
fun mkS (n,acc) = 
    if n < 0 then acc
    else mkS (n-1, chr (n mod 256) :: acc)

val _ = okEq "test6a" string "hello world"
val _ = okEq "test6b" string (implode(mkS(100,nil)))
val _ = okEq "test6c" string (implode(mkS(1000,nil)))

(* Char *)
val _ = okEq "test7a" char #"a"
val _ = app (okEq "test7b" char) (mkS(258,nil))

(* Time *)
val _ = okEq' tm_eq "test8a" time (Time.now())
val _ = okEq "test8b" time Time.zeroTime
val _ = okEq "test8c" time (Time.fromSeconds 0)
val _ = okEq "test8d" time (Time.fromSeconds 100)

(* Real *)
val _ = okEq' Real.== "test9a" real 0.0
val _ = okEq' Real.== "test9b" real 1.1
val _ = okEq' Real.== "test9c" real ~1.1
val _ = okEq' Real.== "test9d" real ~1.1E~20
val _ = okEq' Real.== "test9e" real ~1.1E20
val _ = okEq' Real.== "test9f" real 1.1E~20
val _ = okEq' Real.== "test9g" real 1.1E20
val _ = okEq' Real.== "test9h" real Math.pi
val _ = okEq' Real.== "test9i" real (~Math.pi)
val _ = okEq' Real.== "test9j" real Math.e
val _ = okEq' Real.== "test9k" real (~Math.e)
val _ = okEq' Real.== "test9l" real Real.posInf
val _ = okEq' Real.== "test9m" real Real.negInf
val _ = okEq' Real.== "test9n" real Real.posInf
(*
val _ = okEq' Real.== "test9o" real Real.maxFinite
val _ = okEq' Real.== "test9p" real Real.minPos
val _ = okEq' Real.== "test9q" real Real.minNormalPos
*)

(* Unit *)
val _ = okEq "test10a" unit ()
val _ = okEq "test10b" (pairGen(unit,int)) ((),5)
val _ = okEq "test10c" (pairGen(int,unit)) (5,())
val _ = okEq "test10d" (listGen unit) [(),(),()]

(* PairGen *)
val _ = okEq "test11a" (pairGen(int,bool)) (3,false)
val _ = okEq "test11b" (pairGen(int,bool)) (maxInt,true)
val _ = okEq "test11c" (pairGen(pairGen(int,bool),int)) ((3,false),8)
val _ = okEq "test11d" (pairGen(int,pairGen(int,bool))) (18,(3,false))

(* tup3Gen *)
val _ = okEq "test12a" (tup3Gen(int,bool,int)) (3,false,8)
val _ = okEq "test12b" (tup3Gen(int,bool,pairGen(int,bool))) (3,false,(2,true))
val _ = okEq "test12c" (pairGen(int,tup3Gen(int,bool,pairGen(int,bool)))) (23,(3,false,(2,true)))

(* tup4Gen *)
val _ = okEq "test13a" (tup4Gen(word,int,bool,int)) (0w2,3,false,8)
val _ = okEq "test13b" (tup4Gen(word,int,bool,pairGen(int,bool))) (0w23,3,false,(2,true))
val _ = okEq "test13c" (pairGen(int,tup4Gen(word,int,bool,pairGen(int,bool)))) (23,(0w12,3,false,(2,true)))

(* refGen *)
val _ = okEq' (fn (a,b) => !a = !b) "test14a" (refGen 0 int) (ref 0)
val _ = okEq' (fn (a,b) => !a = !b) "test14b" (refGen 0 int) (ref 7)
val _ = okEq' (fn (a,b) => !a = !b) "test14c" (refGen 0 int) (ref ~7)
val _ = okEq' (fn (a,b) => !a = !b) "test14d" (refGen (0,false) (pairGen(int,bool))) (ref (~7,true))
val _ = okEq' (fn ((a1,a2),(b1,b2)) => !a1 = !b1 andalso !a2 = !b2) "test14e" 
    (pairGen(refGen 0 int, refGen true bool)) (ref ~7,ref false)

(* ref0Gen *)
val _ = okEq' (fn (a,b) => !a = !b) "test15a" (ref0Gen int) (ref 0)
val _ = okEq' (fn (a,b) => !a = !b) "test15b" (ref0Gen int) (ref 7)
val _ = okEq' (fn (a,b) => !a = !b) "test15c" (ref0Gen int) (ref ~7)
val _ = okEq' (fn (a,b) => !a = !b) "test15d" (ref0Gen (pairGen(int,bool))) (ref (~7,true))
val _ = okEq' (fn ((a1,a2),(b1,b2)) => !a1 = !b1 andalso !a2 = !b2) "test15e" 
    (pairGen(ref0Gen int, ref0Gen bool)) (ref ~7,ref false)

(* listGen *)
val _ = okEq "test20a" (listGen int) [~1,2,1,2,4,0,5,6,7,8]
val _ = okEq "test20b" (listGen int) [~1,2000,1100,~2000,~400000,5,6000,7,8,0]
val _ = okEq "test20c" (listGen int) [maxInt,minInt]
val _ = okEq "test20d" (listGen(pairGen(int,bool))) [(1,true),(2,false),(~34,true),(0,false)]
val _ = okEq "test20e" (listGen(pairGen(int,bool))) [(maxInt,true),(minInt,false)]
val _ = okEq "test20f" (pairGen(int,listGen(pairGen(int,bool)))) (34,[(maxInt,true),(minInt,false)])
val _ = okEq "test20g" (pairGen(int,listGen(pairGen(int,bool)))) (2333,[(1,true),(2,false),(~34,true),(0,false)])
val _ = okEq "test20h" (pairGen(listGen(pairGen(int,bool)),int)) ([(1,true),(2,false),(~34,true),(0,false)], ~333)
val _ = okEq "test20i" (pairGen(int,listGen char)) (~2333,mkS(100,nil))
val _ = okEq "test20j" (pairGen(int,listGen char)) (2333,mkS(1000,nil))
(*
val _ = okEq "test20k" (pairGen(int,listGen char)) (~2333,mkS(2000,nil))
val _ = okEq "test20l" (pairGen(int,listGen char)) (~2333,mkS(100000,nil)) 
*)

val _ = okEq "test20m" (pairGen(int,listGen char)) (~2333,nil)
val _ = okEq "test20n" (pairGen(listGen char,int)) (nil,~2)
val _ = okEq "test20o" (listGen int) nil
val _ = okEq "test20p" (listGen char) nil

(* optionGen *)
val _ = okEq "test21a" (optionGen int) NONE
val _ = okEq "test21b" (optionGen int) (SOME 23)
val _ = okEq "test21c" (optionGen int) (SOME 0)
val _ = okEq "test21d" (optionGen int) (SOME ~23)
val _ = okEq "test21e" (optionGen char) NONE
val _ = okEq "test21f" (optionGen char) (SOME #"a")
val _ = okEq "test21g" (optionGen(pairGen(char,int))) NONE
val _ = okEq "test21h" (optionGen(pairGen(char,int))) (SOME(#"a",34))
val _ = okEq "test21i" (pairGen(int,optionGen(pairGen(char,int)))) (23,SOME(#"a",34))
val _ = okEq "test21j" (pairGen(optionGen(pairGen(char,int)),int)) (SOME(#"a",34),34)

(* vectorGen *)
val _ = okEq "test22a" (vectorGen int) (Vector.fromList nil)
val _ = okEq "test22b" (vectorGen int) (Vector.fromList [1,2,3,4,5,6,maxInt,minInt])
val _ = okEq "test22c" (pairGen(int,vectorGen int)) (34,Vector.fromList [1,2,3,4,5,6,maxInt,minInt])
val _ = okEq "test22d" (pairGen(vectorGen int,int)) (Vector.fromList [1,2,3,4,5,6,maxInt,minInt], ~33)

(* shareGen - test it preserves semantics *)
val _ = okEq "test23a" (shareGen(vectorGen int)) (Vector.fromList nil)
val _ = okEq "test23b" (shareGen(optionGen(pairGen(char,int)))) (SOME(#"a",34))

(* enumGen *)
datatype enum = E1 | E2 | E3 | E4 | E5
val pu_enum = enumGen("enum",[E1,E2,E3,E4,E5])
val _ = okEq "test24a" pu_enum E1
val _ = okEq "test24b" pu_enum E2
val _ = okEq "test24c" pu_enum E3
val _ = okEq "test24d" pu_enum E4
val _ = okEq "test24e" pu_enum E5
val _ = okEq "test24f" (listGen pu_enum) [E5,E4,E3,E3,E2,E1,E5]

(* dataGen *)
datatype tree = T of tree * tree | L of string
local
    fun toInt (T _) = 0
      | toInt (L _) = 1
    fun fun_T pu = con1 T (fn T p => p | _ => raise Fail "T") (pairGen(pu,pu))
    fun fun_L pu = con1 L (fn L s => s | _ => raise Fail "L") string
in
    val pu_tree = dataGen("tree",toInt,[fun_T,fun_L])
end

val _ = okEq "test25a" pu_tree (L "hej")
val _ = okEq "test25b" pu_tree (T(L"a",L"b"))
val _ = okEq "test25c" pu_tree (T(L"a",T(L"b",L"c")))
val _ = okEq "test25d" pu_tree (T(T(L"a",L"b"),T(L"c",L"d")))
val _ = okEq "test25e" (pairGen(int,pu_tree)) (3,T(T(L"a",L"b"),T(L"c",L"d")))
val _ = okEq "test25e" (pairGen(pu_tree,int)) (T(T(L"a",L"b"),T(L"c",L"d")),43)

(* data2Gen *)
datatype atree = Ta of atree * btree | La of string
     and btree = Tb of btree * atree | Lb of int | Nb
local
    fun toInta (Ta _) = 0
      | toInta (La _) = 1
    fun toIntb (Tb _) = 0
      | toIntb (Lb _) = 1
      | toIntb Nb = 2
    fun fun_Ta (pu_a,pu_b) = con1 Ta (fn Ta p => p | _ => raise Fail "Ta") (pairGen(pu_a,pu_b))
    fun fun_La (pu_a,pu_b) = con1 La (fn La s => s | _ => raise Fail "La") string
    fun fun_Tb (pu_a,pu_b) = con1 Tb (fn Tb p => p | _ => raise Fail "Tb") (pairGen(pu_b,pu_a))
    fun fun_Lb (pu_a,pu_b) = con1 Lb (fn Lb s => s | _ => raise Fail "Lb") int
    val fun_Nb = con0 Nb
in
    val (pu_atree,pu_btree) = 
	data2Gen("atree", toInta, [fun_Ta,fun_La], 
		 "btree", toIntb, [fun_Tb,fun_Lb,fun_Nb])
end

val _ = okEq "test26a" pu_atree (La "hej")
val _ = okEq "test26b" pu_atree (Ta(La"a",Lb 1))
val _ = okEq "test26c" pu_atree (Ta(La"a",Tb(Lb 1,La"c")))
val _ = okEq "test26d" pu_atree (Ta(Ta(La"a",Lb 1),Tb(Lb 2,La"d")))
val _ = okEq "test26e" (pairGen(int,pu_atree)) (3,Ta(Ta(La"a",Lb 1),Tb(Nb,La"d")))
val _ = okEq "test26f" (pairGen(pu_atree,int)) (Ta(Ta(La"a",Nb),Tb(Lb 2,La"d")),34)
val _ = okEq "test26g" pu_btree (Lb 1)
val _ = okEq "test26h" pu_btree Nb
val _ = okEq "test26i" pu_btree (Tb(Lb 1,La "a"))
val _ = okEq "test26j" pu_btree (Tb(Nb,La "a"))

(* Convert *)
val pu_record = 
    convert (fn (a,b,c) => {a=a,b=b,c=c},
	     fn {a=a,b=b,c=c} => (a,b,c)) (tup3Gen(int,bool,int))

val _ = okEq "test27a" pu_record {a=3,b=true,c=34}
val _ = okEq "test27b" (pairGen(int,pu_record)) (232,{a=3,b=true,c=34})
val _ = okEq "test27c" (pairGen(pu_record,int)) ({a=3,b=true,c=34},2323)

(* Cache:28 *)

(* Register:29 *)
local
   val registered_refs = [ref 10,ref 11,ref 12]
   val pu_intref = Pickle.register "intref" registered_refs (Pickle.refGen 0 Pickle.int)
   val pu_intreflist = Pickle.listGen pu_intref
   val refs = [ref 1,ref 2] @ registered_refs @ [ref 100, ref 101]
   fun eq0 (nil,nil) = true
     | eq0 (x::xs,y::ys) = !x = !y andalso eq0(xs,ys)
     | eq0 _ = false
   fun sel (x::xs, 0) = x
     | sel (x::xs, n) = sel(xs,n-1)
     | sel _ = raise Fail "sel"
   fun sel_eq(xs,ys,n) = sel(xs,n) = sel(ys,n)
   fun sel_noteq(xs,ys,n) = sel(xs,n) <> sel(ys,n)
   fun eq (xs,ys) = 
       eq0(xs,ys) andalso
       sel_eq(xs,ys,2) andalso
       sel_eq(xs,ys,3) andalso
       sel_eq(xs,ys,4) andalso 
       sel_noteq(xs,ys,0) andalso
       sel_noteq(xs,ys,1) andalso
       sel_noteq(xs,ys,5) andalso
       sel_noteq(xs,ys,6)
in
   val _ = okEq' eq "test29a" pu_intreflist refs
end

(* RegisterEq:30 *)

(* Cycles *)

local
    datatype node = Node of int * node list ref
    type graph = node list

    fun eval f (seen, nil, acc) = (seen,acc)
      | eval f (seen, Node(i,ref nodes)::ns, acc) = 
	let fun member x nil = false
	      | member x (y::ys) = x = y orelse member x ys
	    val (seen,acc) = 
		if member i seen then (seen,acc)
		else eval f (i::seen,nodes,f(i,acc))
	in eval f (seen,ns,acc)
	end

    val pu_node =
	let fun fun_node pu = con1 Node (fn Node a => a) 
	    (pairGen(int,refGen nil (listGen pu)))
	in dataGen("node",fn _ => 0, [fun_node])
	end		

    val node : unit -> node =
	let val c = ref 0
	in fn () => Node(!c,ref nil) before c := !c + 1
	end
    infix --
    fun n1 -- n2 = 
	let val Node(_,r1) = n1
	    val Node(_,r2) = n2
	in r1 := n2 :: !r1 
	    ; r2 := n1 :: !r2
	end
    
    val n1 = node()
    val n2 = node()
    val n3 = node()
    val n4 = node()

    fun sum n = eval (op +) (nil,[n],0) 
    fun nodeEquiv (n1,n2) = sum n1 = sum n2
in
    val _ = okEq' nodeEquiv "40a" pu_node n1
    val _ = okEq' nodeEquiv "40b" pu_node n2
    val _ = okEq' nodeEquiv "40c" pu_node n3
    val _ = okEq' nodeEquiv "40d" pu_node n4

    val _ = (  n1 -- n2
	     ; n2 -- n3
	     ; n3 -- n4
	     ; n4 -- n2
	     ; n3 -- n1
	     )

    val _ = okEq' nodeEquiv "41a" pu_node n1
    val _ = okEq' nodeEquiv "41b" pu_node n2
    val _ = okEq' nodeEquiv "41c" pu_node n3
    val _ = okEq' nodeEquiv "41d" pu_node n4
end

end
