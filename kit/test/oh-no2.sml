    (* The balance of a tree is 'L', if the left subtree is one
       deeper than the right subtree, 'B' if the left and right subtrees
       have the same depth, and 'R' if the right subtree is one deeper than
       the left subtree: *)

    datatype 'b map = E | N of 'b map * 'b map 

    exception Impossible of string

    fun plus (t1:'b map, t2:'b map) : 'b map =
      case t2 of
	E => t1
      | N(l,r) => plus 
	    (let fun repl E = raise Impossible "AVLupdate.repl"
		   | repl (N(l,r)) = N(repl l, r)
	     in repl t1
	     end, 
	     l)
