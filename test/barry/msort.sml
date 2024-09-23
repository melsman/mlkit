(* This is tmergesort taken from Paulson's book , page 99 *)

local
  val a = 167
  val m = 2147
in
  fun nextrand seed =
      let val t = a*seed
      in t - (m*(t div m))
      end
  fun randlist (n,seed,tail)=
      if n=0 then (seed,tail)
      else randlist (n-1, nextrand seed, seed::tail)
end

fun merge ([], ys) = (ys:int list)
  | merge (xs, []) = xs
  | merge (l as x::xs, r as y:: ys) =
    if x <= y then x :: merge (xs,r)
    else y :: merge (l,ys)

fun msort [] = []
  | msort [x] = [x]
  | msort xs =
      let val k = length xs div 2
      in merge(msort(List.take(xs,k)),
               msort(List.drop(xs,k)))
      end

fun sorted (x :: (xs as y:: _)) = x <= y andalso sorted xs
  | sorted _ = true

val result =
    let val n = 100000
        val () = print ("Generating list of " ^ Int.toString n ^ " random ints...\n")
        val xs = #2(randlist(n,1,[]))
        val () = print "Sorting...\n"
        val ys = msort xs
    in if length ys = n andalso sorted ys
       then print ("Ok!\n")
       else print ("Err\n")
    end
