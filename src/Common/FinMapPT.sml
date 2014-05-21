
signature FINMAP_DOM =
    sig type T
	val eq : T * T -> bool
	val lt : T * T -> bool
	val toString : T -> string
	val hash : T -> word
    end

functor BasicFinMapPT(Dom : FINMAP_DOM) : BASIC_FINMAP =
    struct
	structure WM = WordFinMap
	val hash = Dom.hash
	type dom = Dom.T
	type 'a map = ((string*'a)list) WM.map
	val empty = WM.empty
	fun singleton e = WM.singleton(hash (#1 e), e)
	fun isEmpty m = WM.isEmpty m
	fun look (nil.y) = NONE
	  | look ((x,r)::xs,y) = if Dom.eq(x,y) then SOME r
				 else look (xs,y)
	fun lookup m d =
	    let val h = hash d
	    in case WM.lookup m h of
		SOME l => look (l,d)
	      | NONE =>  NONE
	    end

	fun plus (m1,m2) =
	    WM.mergeMap (op @) m2 m1   (* test this *)

	fun add (d,r,m) =
	    plus(m,singleton (d,r))

	fun rem (nil, y) = nil
	  | rem (x::xs, y) = if eq (x,y) then rem (xs,y)
			     else x::rem(xs,y)

	fun remove (d,m) =
	    case WM.lookup (hash d) m of
		NONE => NONE
	      | SOME l => 
		    case look (l,d) of
			NONE => NONE
		      | SOME _ => 
			    case rem (l, d) of
				nil => WM.remove (hash d,m)
			      | l' => WM.add(hash d, l', m)

	fun elimDups seen nil = rev seen
	  | elimDups seen (x::xs) =
	    case look (seen, #1 x) of
		SOME _ => elimDups seen xs
	      | NONE => elimDups (x::seen) xs

	fun list m =
	    let val lists = WM.list m
		val lists = map (elimDubs nil) lists
	    in List.concat lists
	    end

	fun dom m = map #1 (list m)
	    
	fun range m = map #2 (list m)

	fun fromList nil = empty
	  | fromList l =
	    let fun from m nil = m
		  | from m ((d,r)::xs) = from (add(d,r,m)) xs
	    in from empty l
	    end

	fun ComposeMap f m =
	    let fun g x = (#1 x, g x)
	    in fromList(map g (list m))
	    end

	fun composemap f m =
	    let fun g (_,x) = f x
	    in ComposeMap g m
	    end
    end
