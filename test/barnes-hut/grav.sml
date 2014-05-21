(* grav.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * Gravity module for hierarchical N-body code; routines to compute gravity.
 *)

signature GRAV =
  sig

    structure S : SPACE
    structure V : VECTOR
      sharing S.V = V

    val hackGrav : {body:S.body, root:S.node, rsize:real, tol:real, eps : real}
	  -> {n2bterm:int, nbcterm:int, skipSelf:bool}

  end; (* GRAV *)

functor Grav (S : SPACE) : GRAV =
  struct

    structure S = S
    structure V = S.V

    fun walk {acc0, phi0, pos0, pskip, eps, rsize, tol, root} = let
	  val skipSelf = ref false
	  val nbcterm = ref 0 and n2bterm = ref 0
	  val tolsq = (tol * tol)
	(* compute a single body-body or body-cell interaction. *)
	  fun gravsub (S.Empty, phi0, acc0, _) = (phi0, acc0)
	    | gravsub (p as S.Node{mass, pos, cell, ...}, phi0, acc0, memo) = let
		val (dr, drsq) = (case memo
		       of NONE => let
			    val dr = V.subv(!pos, pos0)
			    in
			      (dr, V.dotvp(dr, dr) + (eps*eps))
			    end
			| SOME(dr', drsq') => (dr', drsq' + (eps*eps))
		      (* end case *))
		val phii = !mass / (Math.sqrt drsq)
		in
		  case cell
		   of (S.Cell _) => nbcterm := !nbcterm + 1
		    | _ => n2bterm := !n2bterm + 1
		  (* end case *);
		  (phi0 - phii, V.addv(acc0, V.mulvs(dr, phii / drsq)))
		end (* gravsub *)
	(* recursive routine to do hackwalk operation. This combines the
	 * subdivp and walksub routines from the C version.
	 *)
	  fun walksub (p, dsq, phi0, acc0) = (
(*print(implode["    walksub: dsq = ", makestring dsq, ", ", S.prNode p, "\n"]);*)
		case p
		 of S.Empty => (phi0, acc0)
		  | (S.Node{cell = S.BodyCell body, ...}) => 
		      if S.eqBody(body, pskip)
			then (skipSelf := true; (phi0, acc0))
			else gravsub (p, phi0, acc0, NONE)
		  | (S.Node{cell = S.Cell a, pos, ...}) => let
		      val dr = V.subv(!pos, pos0)
		      val drsq = V.dotvp(dr, dr)
		      in
			if ((tolsq * drsq) < dsq)
			  then let (* open p up *)
			    fun loop (i, phi0, acc0) = if (i < S.nsub)
				  then let
				    val (phi0', acc0') = walksub (
					    Array.sub(a, i), dsq/4.0, phi0, acc0)
				    in
				      loop (i+1, phi0', acc0')
				    end
				  else (phi0, acc0)
			    in
			      loop (0, phi0, acc0)
			    end
			  else gravsub (p, phi0, acc0, SOME(dr, drsq))
		      end
		(* end case *))
	  val (phi0, acc0) = walksub (root, rsize*rsize, phi0, acc0)
	  in
	    { phi0 = phi0, acc0 = acc0,
	      nbcterm = !nbcterm, n2bterm = !n2bterm, skip = !skipSelf
	    }
	  end (* walk *)

  (* evaluate grav field at a given particle. *)
    fun hackGrav {body as S.Body{pos, phi, acc, ...}, root, rsize, eps, tol} = let
	  val {phi0, acc0, nbcterm, n2bterm, skip} = walk {
		  acc0 = V.zerov, phi0 = 0.0, pos0 = !pos, pskip = body,
		  eps = eps, rsize = rsize, tol = tol, root = root
		}
	  in
	    phi := phi0;
	    acc := acc0;
(**
app (fn (fmt, items) => print(Format.format fmt items)) [
  ("pos = [%f %f %f]\n", map Format.REAL (V.explode(!pos))),
  ("pos = [%f %f %f]\n", map Format.REAL (V.explode acc0)),
  ("phi = %f\n", [Format.REAL phi0])
];
raise Fail "";
**)
	    {nbcterm=nbcterm, n2bterm=n2bterm, skipSelf=skip}
	  end (* hackgrav *)

  end; (* Grav *)
