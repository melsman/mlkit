(*$EqSetList*)

(* sets of integers represented as sorted lists, without repetitions*)

structure EqSetList (*: EQ_SET where type 'a Set = int list *)=  
  struct
       type 'a Set = int list
       fun insert (x:int) l =
             let fun loop [] = [x]
                   | loop (l as (y::ys)) = if x<y then x::l
                                           else if x=y then l
                                           else y:: loop(ys)
             in loop l 
             end
       fun singleton (n: int) = [n]
       val empty = []
       fun isEmpty [] = true
         | isEmpty _ = false
       exception Empty of string
       fun select(op :: p) = p
         | select _ = raise Empty "select"
       fun union'([], l2) = l2
         | union'(l1, []) = l1
         | union'(l1 as (x::xs), l2 as (y::ys)) =
             if (x:int)<y then x::union'(xs,l2)
             else if x=y then x :: union'(xs, ys)
             else y::union'(l1,ys)
       fun union l1 l2 = union'(l1,l2)
       fun insert x xs = union'([x],xs)
       fun remove x [] = []
         | remove (x:int) (x'::xs) = if x=x' then xs else remove x xs

       fun diff(l1, []) = l1
         | diff([], l2) = []
         | diff(l1 as (x::xs), l2 as (y::ys)) =
             if (x:int)<y then x::diff(xs, l2)
             else if x=y then diff(xs, ys)
             else (* x>y *) diff(l1, ys)
       fun difference l1 l2 = diff(l1,l2)             
       fun int(l1,[]) = []
         | int([],l2) = []
         | int(l1 as (x::xs), l2 as (y::ys)) =
             if (x:int)<y then int(xs, l2)
             else if x=y then x ::int(xs, ys)
             else (* x>y *) int(l1, ys)
       fun intersect l1 l2 = int(l1,l2)
       fun size l = List.size l
       fun list l = l
       fun fromList l = l (* assumes l already sorted!! *)
       fun eq(l1,l2: int list) = l1 = l2

       exception EqSetList_NOT_IMPLEMENTED
       fun partition _ _ = raise EqSetList_NOT_IMPLEMENTED
       fun closure _ = raise EqSetList_NOT_IMPLEMENTED
       fun map _ = raise EqSetList_NOT_IMPLEMENTED
       fun apply _ = raise EqSetList_NOT_IMPLEMENTED
       fun fold _ = raise EqSetList_NOT_IMPLEMENTED
       fun fold' _ = raise EqSetList_NOT_IMPLEMENTED
    end

