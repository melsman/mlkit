(* fun removeConsDup [] = [] *)
(*   | removeConsDup [x] = [x] *)
(*   | removeConsDup (x::y::xs) = *)
(* 	if x = y then removeConsDup (y::xs) *)
(* 	else x :: removeConsDup (y::xs) *)

(* fun append [] ys = ys *)
(*   | append (x::xs) ys = x :: append xs ys *)


(* fun appendEnd [] = ["end"] *)
(*   | appendEnd (x::xs) = x :: appendEnd xs *)

(* fun prependStart l = "start" :: l *)

(* fun transform [] = [] *)
(*   | transform (x::xs) = (x + 1) :: transform xs *)

(* fun run () = *)
(*   let val data = [1, 2, 3, 4, 5] *)
(* 	  val result = transform data *)
(*   in resetRegions data; *)
(* 	length result *)
(*   end *)

(* fun cp [] = [] *)
(* | cp (x::xs) = x :: cp xs *)
(* (* exormorphic merge *) *)
(* fun merge(xs, []) : int list = cp xs *)
(* | merge([], ys) = cp ys *)
(* | merge(l1 as x::xs, l2 as y::ys) = *)
(* if x<y then x :: merge(xs, l2) *)
(* else y :: merge(l1, ys) *)
(* (* splitting a list *) *)
(* fun split(x::y::zs, l, r) = split(zs, x::l, y::r) *)
(* | split(x::xs, l, r) = (xs, x::l, r) *)
(* | split(p as ([], l, r)) = p *)
(* (* exomorphic merge sort *) *)
(* fun msort [] = [] *)
(* | msort [x] = [x] *)
(* | msort xs = let val (_, l, r) = split(xs, [], []) *)
(* in resetRegions xs; *)
(* merge(msort l before resetRegions l, *)
(* msort r before resetRegions r) *)
(* end *)

(* local *)
(*   fun consume (xs : int list, ys : int list) = *)
(* 	  let val (n, ) *)
(* 	in resetRegions xs; *)
(* 	   n + length ys *)
(* 	end *)
(* in *)
(*   val res = consume ([1, 2, 3], [4, 5, 6]) *)
(* end *)

(* val _ = consume ([1, 2, 3], [4, 5, 6]) *)


(* local *)
(*   fun shrink [] = [] *)
(* 	| shrink [x] = [] *)
(* 	| shrink (x::_::xs) = x :: shrink xs *)

(*   fun loop [] = 0 *)
(* 	| loop xs = *)
(* 		let val xs' = shrink xs *)
(* 		in resetRegions xs; 1 + loop xs' *)
(* 		end *)
(* in *)
(*   val res = loop [1, 2, 3, 4, 5, 6, 7, 8, 9] *)
(* end *)


(* local *)
(*   fun shrink [] = [] *)
(* 	| shrink [x] = [] *)
(* 	| shrink (x::_::xs) = x :: shrink xs *)

(*   fun loop`[r] [] = 0 *)
(* 	| loop`[r] (xs : int list`r) = *)
(* 		let val xs' = shrink xs *)
(* 		in resetRegions`[r] (); *)
(* 		   1 + loop xs' *)
(* 		end *)
(* in *)
(*   val res = loop [1, 2, 3, 4, 5, 6, 7, 8, 9] *)
(* end *)

(* val _ = prim ("callSbrk", ()) *)

(* fun test () = *)
(*     let with r *)
(* 	  val before_bytes = prim `[r] ("get_Region_Memory_Usage_Bytes", ()) *)
(* 	  val page_size = prim ("get_Page_Size_Bytes", ()) *)
(* 	  val pi : real`r = Real.fromInt 314244 / Real.fromInt 100000 *)
(* 	  val after_bytes = prim `[r] ("get_Region_Memory_Usage_Bytes", ()) *)
(* 	in *)
(* 	  print ("Memory usage before: " ^ Int.toString before_bytes ^ " bytes\n"); *)
(* 	  print ("Memory usage after: " ^ Int.toString after_bytes ^ " bytes\n"); *)
(* 	  print ("Difference: " ^ Int.toString (after_bytes - before_bytes) ^ " bytes\n"); *)
(* 	  print ("Pi is: " ^ Real.toString pi ^ "\n") *)
(* 	end *)

(* val _ = test () *)
(* fun test () = *)
(*     let with r *)
(*       val l : int list`r = [1, 2, 3, 4, 5] *)
(*       val _ = Size.size (Size.list Size.int) l *)
(*       val u = Regions.memoryUsageOfRegion `[r] () *)
(*     in print (Int.toString u ^ "\n"); ignore l *)
(*     end *)

(* val _ = test () *)

(* fun test () = *)
(*     let with r1 r2 *)
(*       val l1 : int list`r1 = [1, 2, 3, 4, 5] *)
(* 	  val l2 : int list`r2 = [6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25] *)
(*       val n1 = prim `[r1] ("get_Region_Memory_Usage_Bytes", ()) *)
(* 	  val n2 = prim `[r2] ("get_Region_Memory_Usage_Bytes", ()) *)
(*     in *)
(* 	  print ("l1: " ^ String.concatWith ", " (List.map Int.toString l1) ^ "\n"); *)
(* 	  print ("l2: " ^ String.concatWith ", " (List.map Int.toString l2) ^ "\n"); *)
(*       print ("Memory usage of region 1: " ^ Int.toString n1 ^ " bytes\n"); *)
(* 	  print ("Memory usage of region 2: " ^ Int.toString n2 ^ " bytes\n") *)
(* 	end *)

(* val _ = test () *)

fun f `[r1 r2] (x: {a: int, b: string`r2, c:int}`r1) : int =
  let
	val {a, b, c} = x
  in
	a + String.size b + c
  end
