(* space.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * The quad/oct-tree representation of space.
 *)

signature SPACE =
  sig

    structure V : VECTOR

    datatype body = Body of {
	mass : real,
	pos : real V.vec ref,
	vel : real V.vec ref,
	acc : real V.vec ref,
	phi : real ref
      }

    datatype cell
      = BodyCell of body
      | Cell of node Array.array

    and node
      = Empty
      | Node of {
	  mass : real ref,
	  pos : real V.vec ref,
	  cell : cell
	}

    datatype space = Space of {
	rmin : real V.vec,
	rsize : real,
	root : node
      }

    val nsub : int	(* number of sub cells / cell (2 ^ V.dim) *)

    val putCell : (cell * int * node) -> unit
    val getCell : (cell * int) -> node
    val mkCell : unit -> cell
    val mkBodyNode : body -> node
    val mkCellNode : cell -> node
    val eqBody : body * body -> bool

  (* debugging code *)
    val dumpTree : node -> unit
    val prBody : body -> string
    val prNode : node -> string

  end; (* SPACE *)

functor Space (V : VECTOR) : SPACE =
  struct

    structure V = V

    datatype body = Body of {
	mass : real,
	pos : real V.vec ref,
	vel : real V.vec ref,
	acc : real V.vec ref,
	phi : real ref
      }

    datatype cell
      = BodyCell of body
      | Cell of node Array.array

    and node
      = Empty
      | Node of {
	  mass : real ref,
	  pos : real V.vec ref,
	  cell : cell
	}

    datatype space = Space of {
	rmin : real V.vec,
	rsize : real,
	root : node
      }

    fun eqBody(Body{mass,pos,vel,acc,phi},
	       Body{mass=m1,pos=p1,vel=v1,acc=a1,phi=h1}) = 
      (Real.==(mass, m1) andalso Real.==(!phi, !h1)
       andalso V.equal(!pos, !p1) andalso V.equal(!vel, !v1)
       andalso V.equal(!acc, !a1))

  (* number of sub cells per cell (2 ^ V.dim) *)
    val nsub = Word.toInt(Word.<<(0w1, Word.fromInt V.dim))

    fun putCell (Cell a, i, nd) = Array.update(a, i, nd)
    fun getCell (Cell a, i) = Array.sub(a, i)
    fun mkCell () = Cell(Array.array(nsub, Empty))
    fun mkBodyNode (body as Body{pos, mass, ...}) = Node{
	    cell = BodyCell body,
	    mass = ref mass,
	    pos = ref (!pos)
	  }
    fun mkCellNode cell = Node{cell = cell, mass = ref 0.0, pos = ref V.zerov}

  (* debugging code *)
    local
      val rfmt = Real.toString
      val vfmt = V.format{lp="[", rp="]", sep=",", cvt = rfmt}
    in
    fun prBody (Body{mass, pos, vel, acc, phi}) = String.concat [
	    "B{m=", rfmt mass,
	    ", p=", vfmt(!pos),
	    ", v=", vfmt(!vel),
	    ", a=", vfmt(!acc),
	    ", phi=", rfmt(!phi), "}"
	  ]
    fun prNode Empty = "Empty"
      | prNode (Node{mass, pos, cell}) = let
	  val cell = (case cell
		 of (Cell _) => "Cell"
		  | (BodyCell b) => (*prBody b*) "Body"
		(* end case *))
	  in
	    String.concat [
		"N{m=", rfmt(!mass),
		", p=", vfmt(!pos),
		cell, "}"
	      ]
	  end
    end

    fun dumpTree tree = let
	  fun printf items = TextIO.output(TextIO.stdOut, String.concat items)
	  fun indent i = StringCvt.padLeft #" " (i+1) ""
	  fun dump (node, l) = let
		fun dump' (Node{cell=Cell a, ...}) = let
		      fun dump'' i = (dump(Array.sub(a, i), l+1); dump''(i+1))
		      in
			(dump'' 0) handle _ => ()
		      end
		  | dump' _ = ()
		in
		  printf [
		      StringCvt.padLeft #" " 2 (Int.toString l),
		      indent l,
		      prNode node, "\n"
		    ];
		  dump' node
		end
	  in
	    dump (tree, 0)
	  end

  end; (* Space *)

