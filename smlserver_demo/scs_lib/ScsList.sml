signature SCS_LIST =
  sig
 
   (* [zip xe ye xs ys] a special version of the List.zip function where the two
      lists doesn't have to be of the same length. xe is used as element
      if size xs < size ys and ye is used as element if size ys < size
      xs. *)
   val zip : 'a -> 'b -> 'a list -> 'b list -> ('a * 'b) list

    (* [allDifferent compare xs] returns true if all elements in xs
       are different using the compare function. *)
    val allDifferent : ('a * 'a -> bool) -> 'a list -> bool

    (* [mkDifferent compare xs] returns a list xs' with all elements
       in xs sorted and all redundant elements removed. The function
       all Different will return true on xs' and a similar compare
       function. *)
    val mkDifferent : ('a * 'a -> General.order) -> 'a list -> 'a list

    (* [allOrNone fn_empty xs] returns true if all elements in xs are
       non empty. The functions fn_empty decides whether an element is
       empty. *)
    val allOrNone    : ('a -> bool) -> 'a list -> bool

      (* [valOf xs] returns the list xs' of elements e where e
         corresponds to an element e_opt = SOME e in xs. *)
    val valOf : 'a option list -> 'a list

   (* [contains x xs] retruns true if atleast one x exists in xs;
       otherwise returns false. *)
    val contains : ''a -> ''a list -> bool

    val union : (''a * ''a -> bool) -> ''a list * ''a list -> ''a list
  end

structure ScsList :> SCS_LIST =
  struct
    fun allDifferent fn_eq ls =
      let
	fun check(c,ls1,ls2) = not (List.exists (fn x => fn_eq(c,x)) (ls1@ls2))
	fun check_all ([],ls2) = true
	  | check_all (x::xs,ls2) = check(x,xs,ls2) andalso check_all(xs,x::ls2)
      in
        check_all (ls,[])
      end

    fun mkDifferent compare xs =
      let
	val xs' = Listsort.sort compare xs
	fun loop [] = []
	  | loop ([x]) = [x]
	  | loop (x1::x2::xs) = 
	  if compare(x1,x2) = General.EQUAL then
	    loop (x1::xs)
	  else
	    x1 :: (loop (x2::xs))
      in
	loop xs'
      end

    fun allOrNone fn_empty ls = List.all fn_empty ls orelse List.all (not o fn_empty) ls

    fun zip xe ye xs ys =
      let 
        fun zip' ([],[]) = []
          | zip' ([],y::ys) = (xe,y) :: zip' ([],ys)
          | zip' (x::xs,[]) = (x,ye) :: zip' (xs,[])
          | zip' (x::xs,y::ys) = (x,y) :: zip' (xs,ys)
      in
        zip' (xs,ys) 
      end

    fun valOf xs = 
      List.foldr (fn (x,acc) => case x of NONE => acc | SOME e => e::acc) [] xs

    fun contains x xs = List.exists (fn y => x = y) xs

    (* A few functions taken from: http://aleph0.clarku.edu/~djoyce/cs170/mlexample3.html *)

    (* membership. member(x,y) should be true when x is an element of the set y *)
    fun member fn_eq (x,[]) = false
      | member fn_eq (x,b::y) =
      if fn_eq(x,b) then true
      else member fn_eq (x,y)

    (* subset. subset(x,y) should be true when every element of the set x is a member of the set y *)
    fun subset fn_eq ([],y) = true
      | subset fn_eq (a::x,y) =
      if member fn_eq (a,y) then subset fn_eq (x,y)
      else false

    (* equal. equal(x,y) should be true when x and y are the same set *)
    fun equal fn_eq (x,y) = subset fn_eq (x,y) andalso subset fn_eq (y,x)

    (* union. union(x,y) should give the union of the two sets x and y *)
    fun union fn_eq  ([],y) = y
      | union fn_eq (a::x,y) =
      if member fn_eq (a,y) then union fn_eq (x,y)
      else a::union fn_eq (x,y)

    (* intersection. intersection(x,y) should give the intersection of the two sets *)
    fun intersection fn_eq ([],y) = []
      | intersection fn_eq (a::x,y) =
      if member fn_eq (a,y) then a::intersection fn_eq (x,y)
      else intersection fn_eq (x,y)

   (* difference. difference(x,y) should give the difference, that is, 
      the elements of the set x that do not belong to the set y *)
    fun difference fn_eq ([],y) = []
      | difference fn_eq (a::x,y) =
      if member fn_eq (a,y) then difference fn_eq (x,y)
      else a::difference fn_eq (x,y)

   (* power. power(x) should give the power set of x. 
      For instance, if x is the set [1,2], then power(x) should be 
      the set of sets [[], [1], [2], [1,2]] *)
(*   local
     fun insert fn_eq (a,[]) = []
       | insert fn_eq (a,b::y) = union fn_eq ([a],b):: insert fn_eq (a,y)
   in
     fun power fn_eq ([]) = [[]]
       | power fn_eq (a::y) = union fn_eq (power fn_eq y,insert fn_eq (a,power fn_eq y))
   end*)
  end
