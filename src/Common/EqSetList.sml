(*$EqSetList*)

(* sets of integers represented as sorted lists, without repetitions*)

structure EqSetList (* more or less EQ_SET where type 'a Set = int list *)=  
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

       datatype inter_size = NONE | ONE of int | MANY of int * int

       fun inc(x, NONE)= ONE x
         | inc(x, b as (ONE y))= if x=y then b else MANY(y,x)
         | inc(x, b as (MANY _)) = b

       fun int'(l1,[],s) = s
         | int'([],l2,s) = s
         | int'(_,_, s as (MANY _ ))= s
         | int'(l1 as (x::xs),l2 as (y::ys),s)=
             if x=y then int'(xs,ys,inc(x,s))
             else if x<y then int'(xs, l2, s)
                  else (* x>y *)int'(l1, ys, s)

       fun showl msg l = (output(std_out, msg ^ List.stringSep "[" "]" ", " Int.string l ^ "\n"))

       fun showsize (NONE) = "NO_INTER"
         | showsize (ONE r) = "SINGLETON: " ^ Int.string r
         | showsize (MANY(r1,r2)) = "MANY(" ^ Int.string r1 ^ "," ^ Int.string r2 ^ ")"

       fun smallInter(l1,l2) = 
        ((*showl "smallInter.l1 = " l1;
         showl "smallInter.l2 = " l2;*)
         let val result = int'(l1,l2,NONE)
         in (*output(std_out, "smallInter.result= " ^ showsize result ^ "\n");*)
            result
         end)

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

