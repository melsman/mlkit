
(* Set functor based on elements that provide equality explicitly. *)

functor EqSetList(structure PP : PRETTYPRINT
		  type elt
		  val eq : elt * elt -> bool) : KIT_MONO_SET =
  struct
    type elt = elt
    type Set = elt list

    val empty : Set = []
    fun singleton (e : elt) : Set = [e]

    fun member (e: elt) (s : Set) : bool =
      let fun mem [] = false
	    | mem (x::xs) = eq(x,e) orelse mem xs
      in mem s
      end

    fun list (s : Set) : elt list =
      let fun f ([],a) = a
	    | f (x::xs,a) = if member x a then f (xs,a)
			    else f (xs, x::a)
      in f (s,[])
      end

    fun size (s : Set) : int = length(list s)

    fun isEmpty ([] : Set) : bool = true
      | isEmpty _ = false

    fun fromList (s: elt list) : Set = s 

    fun addList (es: elt list) (s: Set) : Set = es @ s
      (* addList l s : Add elements in list l to s. *)

    fun insert (e: elt) (s: Set) : Set = e::s

    fun remove (e: elt) (s: Set) : Set =
      let fun rem ([], a) = a
	    | rem (x::xs, a) = if eq (x, e) then rem (xs, a)
			       else rem (xs, x::a)
      in rem (s, [])
      end

    fun difference (s1: Set) (s2: Set) : Set =
      let fun f ([], a) = a 
	    | f (x::xs, a) = if member x s2 then f (xs, a)
			     else f (xs, x::a)
      in f (s1, [])
      end
	
    fun intersect (s1: Set) (s2: Set) : Set =
      let fun f ([], a) = a
	    | f (x::xs, a) = if member x s2 then f (xs, x::a)
			     else f (xs, a)
      in f (s1, [])
      end

    fun union ([] : Set) (s2: Set) : Set = s2
      | union s1 [] = s1
      | union s1 s2 = s1 @ s2

    fun partition (f: elt -> bool) (s: Set) : Set * Set =
      let fun part ([], a1, a2) = (a1,a2)
	    | part (x::xs, a1, a2) = if f x then part (xs, x::a1, a2)
				     else part (xs, a1, x::a2)
      in part (list s, [], [])
      end

      (* subst (e,b) s : Substitute element b in s with element a. *)
    fun subst (e:elt, b:elt) (s : Set) : Set =
      let fun f ([], a) = a
	    | f (x::xs, a) = if eq(e,x) then f (xs, b::a)
			     else f (xs, x::a)
      in f (s, [])
      end

    fun fold (f: elt -> 'b -> 'b) (base : 'b) (s: Set) : 'b =
      foldl (fn (a,b) => f a b) base (list s)

      (* fold f base s; folds using f over the base element. *)

    fun map (f: elt -> elt) (s : Set) : Set = List.map f (list s)
      (* map f s; builds a new set by applying f to each element in s *)

    fun apply (f: elt -> unit) (s: Set) : unit = app f (list s)
      (* apply f s; applies f to each element of s (in order) *)

    type StringTree = PP.StringTree

    fun layoutSet {start: string, sep: string, finish: string} 
      (f : elt -> StringTree) (s: Set) : StringTree =
      PP.NODE{start=start, finish=finish, indent=3, childsep=PP.RIGHT sep,
	      children=List.map f (list s)}

    fun eq (s1: Set) (s2: Set) : bool =
      let val s1 = list s1
	  val s2 = list s2
	  fun f [] = true
	    | f (x::xs) = member x s2 andalso f xs
      in size s1 = size s2 andalso f s1
      end
(*
    fun debug s f a = (print "\n[Enter "; print s; print ".."; 
		       let val b = f a
		       in print "]"; b
		       end)

    fun debug2 s f a1 a2 = (print "\n[Enter "; print s; print ".."; 
			    let val b = f a1 a2
			    in print "]"; b
			    end)

    val eq = debug2 "eq" eq
    val member = debug2 "member" member
    val list = debug "list" list
    val singleton = debug "singleton" singleton
    val layoutSet2 = debug "layoutSet" layoutSet
    val size = debug "size" size
    val isEmpty = debug "isEmpty" isEmpty
    val fromList = debug "fromList" fromList
    val addList = debug "addList" addList
    val insert = debug2 "insert" insert
    val remove = debug2 "remove" remove
    val difference = debug2 "difference" difference
    val intersect = debug2 "intersect" intersect
    val union = debug2 "union" union
    val partition = debug2 "partition" partition
    val subst = debug2 "subst" subst
    val fold = fn a => debug "fold" fold a
    val map = debug2 "map" map
    val apply = debug2 "apply" apply
*)
  end


(* Test : OK  ME 1998-07-22 *)
(*
local
  structure BasicIO = BasicIO()
  structure Crash = Crash(structure BasicIO = BasicIO)
  structure Report = Report(structure BasicIO = BasicIO)
  structure Flags = Flags(structure Crash = Crash
			  structure Report = Report)
  structure PP = PrettyPrint(structure Report = Report
			     structure Crash = Crash
			     structure Flags = Flags)
  structure Set = 
    EqSetList(structure PP = PP
	      type elt = int
	      val eq = op =)
  open Set
  val a = fromList [1,2,3,4,5,3,1]
  val b = fromList [7,3,5,6,4,3,6,7,7]
  val c = fromList [] 
  fun check s (a, b) = if eq a b then print(s ^ " : OK\n") 
		       else print(s ^ " : ERROR\n")
  val _ = if eq a b then print("test0 : ERROR\n")
	  else print("test0 : OK\n")
  val _ = check "test1" (difference a b, fromList[1,2])
  val _ = check "test2" (intersect a b, fromList[3,4,5])
  val _ = check "test3" (union a b, fromList[1,2,3,4,5,6,7])
  val _ = check "test4" (remove 3 a, fromList[1,2,4,5])
  val _ = check "test5" (insert 7 a, fromList[1,2,3,4,5,7]) 
  val _ = check "test6" (insert 3 a, fromList[1,2,3,4,5])
  val _ = check "test7" (subst (3,8) a, fromList[1,2,4,5,8])
  val _ = check "test8" (intersect a c, c)
  val _ = check "test8a" (intersect c a, c)
  val _ = check "test9" (difference a c, a)
  val _ = check "test9a" (difference c a, c)
  val _ = check "test10" (union c a, a)
  val _ = check "test11" (union a c, a)
    
  fun lay s set = PP.outputTree(print, layoutSet {start=s ^ "={", finish="}",sep=","} (PP.LEAF o Int.toString) set, 70)
  val _ = lay "a" a
  val _ = lay "b" b
  val _ = lay "c" c
in
end
*)
