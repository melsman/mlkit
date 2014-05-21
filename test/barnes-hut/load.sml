(* load.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * Code to build the tree from a list of bodies.
 *)

signature LOAD =
  sig

    structure S : SPACE
    structure V : VECTOR
      sharing S.V = V

    val makeTree : (S.body list * real V.vec * real) -> S.space

  end; (* LOAD *)

functor Load (S : SPACE) : LOAD =
 struct

    structure S = S
    structure V = S.V

    exception NotIntCoord

    fun rshift (n, k) = Word.toInt(Word.~>>(Word.fromInt n, Word.fromInt k))

    val IMAX = 0x20000000    (* 2^29 *)
    val IMAXrs1 = rshift(IMAX, 1)
    val rIMAX = real IMAX

  (* compute integerized coordinates.  Raises the NotIntCoord exception,
   * if rp is out of bounds.
   *)
    fun intcoord (rp, rmin, rsize) = let
	  val xsc = V.divvs (V.subv(rp, rmin), rsize)
	  fun cvt x = if ((0.0 <= x) andalso (x < 1.0))
		then floor(rIMAX * x)
		else raise NotIntCoord
	  in
	    V.mapv cvt xsc
	  end

  (* determine which subcell to select. *)
    fun subindex (iv, l) = let
	  fun aux (v, (i, k)) = if (Word.andb(Word.fromInt v, Word.fromInt l) <> 0w0)
		then (i + rshift(S.nsub, k+1), k+1)
		else (i, k+1)
	  in
	    #1 (V.foldv aux iv (0, 0))
	  end

  (* enlarge cubical "box", salvaging existing tree structure. *)
    fun expandBox (nd as S.Body{pos, ...}, box as S.Space{rmin, rsize, root}) = (
	  (intcoord (!pos, rmin, rsize); box)
	    handle NotIntCoord => let
	      val rmid = V.addvs (rmin, 0.5 * rsize)
	      val rmin' = V.map3v (fn (x,y,z) =>
			      if x < y then z - rsize else z) (!pos, rmid, rmin)
	      val rsize' = 2.0 * rsize
	      fun mksub (v, r) = let
		    val x = intcoord (v, rmin', rsize')
		    val k = subindex (x, IMAXrs1)
		    val cell = S.mkCell ()
		    in
		      S.putCell (cell, k, r); cell
		    end
	      val box = (case root
		     of S.Empty => S.Space{rmin=rmin', rsize=rsize', root=root}
		      | _ => S.Space{
			    rmin = rmin',
			    rsize = rsize',
			    root = S.mkCellNode (mksub (rmid, root))
			  }
		    (* end case *))
	      in
	        expandBox (nd, box)
	      end)


  (* insert a single node into the tree *)
    fun loadTree (body as S.Body{pos=posp, ...}, S.Space{rmin, rsize, root}) = let
	  val xp = intcoord (!posp, rmin, rsize)
	  fun insert (S.Empty, _) = S.mkBodyNode body
	    | insert (n as S.Node{cell=S.BodyCell _, pos=posq, ...}, l) = let
		val xq = intcoord (!posq, rmin, rsize)
		val k = subindex (xq, l)
		val a = S.mkCell()
		in
		  S.putCell(a, k, n);
		  insert (S.mkCellNode a, l)
		end
	    | insert (n as S.Node{cell, ...}, l) = let
		val k = subindex (xp, l)
		val subtree = insert (S.getCell (cell, k), rshift(l, 1))
		in
		  S.putCell (cell, k, subtree);
		  n
		end
	  in
	    S.Space{rmin = rmin, rsize = rsize, root = insert (root, IMAXrs1)}
	  end

  (* descend tree finding center-of-mass coordinates. *)
    fun hackCofM S.Empty = ()
      | hackCofM (S.Node{cell = S.BodyCell _, ...}) = ()
      | hackCofM (S.Node{cell = S.Cell subcells, mass, pos}) = let
	  fun sumMass (i, totMass, cofm) = if (i < S.nsub)
		then (case Array.sub(subcells, i)
		   of S.Empty => sumMass (i+1, totMass, cofm)
		    | (nd as S.Node{mass, pos, ...}) => let
			val _ = hackCofM nd
			val m = !mass
			in
			  sumMass (i+1, totMass + m, V.addv(cofm, V.mulvs(!pos, m)))
			end
		  (* end case *))
		else (
		  mass := totMass;
		  pos := V.divvs(cofm, totMass))
	  in
	    sumMass (0, 0.0, V.zerov)
	  end

  (* initialize tree structure for hack force calculation. *)
    fun makeTree (bodies, rmin, rsize) = let
	  fun build ([], space) = space
	    | build ((body as S.Body{mass, ...}) :: r, space) = 
	       if Real.==(mass, 0.0) then build (r, space)
	       else let
		   val box = expandBox (body, space)
		   val box = loadTree(body, box)
		 in build (r, box)
		 end
	  val (space as S.Space{root, ...}) =
	    build (bodies, S.Space{rmin=rmin, rsize=rsize, root=S.Empty})
	  in
	    hackCofM root;
	    space
	  end

  end; (* functor Load *)
