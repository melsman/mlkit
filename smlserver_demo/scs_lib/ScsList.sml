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

   (* [contains x xs] retruns true if atleast one x exists in xs;
       otherwise returns false. *)
    val contains : ''a -> ''a list -> bool
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

    fun contains x xs = List.exists (fn y => x = y) xs
  end
