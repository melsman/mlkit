(* data-io.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * I/O routines for export version of hierarchical N-body code.
 *)

signature DATA_IO =
  sig

    structure S : SPACE

    val inputData : string -> {
	    nbody : int,
	    bodies : S.body list,
	    tnow : real,
	    headline : string
	  }

  (* output routines *)
    val initOutput : {
	    outfile : string, headline : string, nbody : int, tnow : real,
	    dtime : real, eps : real, tol : real, dtout : real, tstop : real
	  } -> unit
    val output : {
	    nbody : int, bodies : S.body list, n2bcalc : int, nbccalc : int,
	    selfint : int, tnow : real
	  } -> unit
    val stopOutput : unit -> unit

  end;

functor DataIO (S : SPACE) : DATA_IO =
  struct

    structure SS = Substring
    structure S = S
    structure V = S.V

    val atoi = valOf o Int.scan StringCvt.DEC SS.getc

  (* NOTE: this really out to be implemented using the lazy IO streams,
   * but SML/NJ doesn't implement these correctly yet.
   *)
    fun inputData fname = let
	  val strm = TextIO.openIn fname
	  val buf = ref(SS.all "")
	  fun getLn () = (case (TextIO.inputLine strm)
		 of "" => raise Fail "inputData: EOF"
		  | s => buf := SS.all s
		(* end case *))
	  fun skipWS () = let
		val buf' = SS.dropl Char.isSpace (!buf)
		in
		  if (SS.isEmpty buf')
		    then (getLn(); skipWS())
		    else buf'
		end
	  fun readInt () = let
		val (n, ss) = atoi (skipWS ())
		in
		  buf := ss; n
		end
	  fun readReal () = let
		val (r, ss) = valOf (Real.scan SS.getc (skipWS()))
                in
                  buf := ss; r
                end
	  val nbody = readInt()
	  val _ = if (nbody < 1)
		then raise Fail "absurd nbody"
		else ()
	  val ndim = readInt()
	  val _ = if (ndim <> V.dim)
		then raise Fail "absurd ndim"
		else ()
	  val tnow = readReal()
	  fun iter f = let
		fun loop (0, l) = l
		  | loop (n, l) = loop (n-1, f() :: l)
		in
		  fn n => loop (n, [])
		end
	  fun readVec () = V.implode (rev (iter readReal ndim))
	  val massList = iter readReal nbody
	  val posList = iter readVec nbody
	  val velList = iter readVec nbody
	  fun mkBodies ([], [], [], l) = l
	    | mkBodies (m::r1, p::r2, v::r3, l) = let
		val b = S.Body{
			mass = m,
			pos = ref p,
			vel = ref v,
			acc = ref V.zerov,
			phi = ref 0.0
		      }
		in
		  mkBodies(r1, r2, r3, b::l)
		end
	  in
	    TextIO.closeIn strm;
	    { nbody = nbody,
	      bodies = mkBodies (massList, posList, velList, []),
	      tnow = tnow,
	      headline = concat["Hack code: input file ", fname, "\n"]
	    }
	  end

    local
      val timer = ref (Timer.startCPUTimer ())
    in
    fun initTimer () = timer := Timer.startCPUTimer()
    fun cputime () = let
	  val {usr, sys, gc} = Timer.checkCPUTimer(!timer)
	  val totTim = Time.+(usr, gc)
	  in
	    (Time.toReal totTim) / 60.0
	  end
    end

    type out_state = {
	tout : real,
	dtout : real,
	dtime : real,
	strm : TextIO.outstream
      }
    val outState = ref (NONE : out_state option)

    fun fprintf (strm, items) = TextIO.output(strm, String.concat items)
    fun printf items = fprintf(TextIO.stdOut, items)
    fun pad n s = StringCvt.padLeft #" " n s
    fun fmtInt (wid, i) = pad wid (Int.toString i)
    fun fmtReal (wid, prec, r) = pad wid (Real.fmt (StringCvt.FIX(SOME prec)) r)
    fun fmtRealE (wid, prec, r) = pad wid (Real.fmt (StringCvt.SCI(SOME prec)) r)
    local
      fun itemFmt r = fmtReal (9, 4, r)
      val fmt = V.format{lp="", sep="", rp="", cvt=itemFmt}
    in
    fun printvec (init, vec) = printf [
	    "\t ", pad 9 init, fmt vec, "\n"
	  ]
    end (* local *)

    fun stopOutput () = (case (! outState)
	   of NONE => ()
	    | (SOME{strm, ...}) => (TextIO.closeOut strm; outState := NONE)
	  (* end case *))

    fun initOutput {outfile, headline, nbody, tnow, dtime, eps, tol, dtout, tstop} = (
	  initTimer();
	  printf ["\n\t\t", headline, "\n\n"];
	  printf (map (pad 12) ["nbody", "dtime", "eps", "tol", "dtout", "tstop"]);
	  printf ["\n"];
	  printf [fmtInt(12, nbody), fmtReal(12, 5, dtime)];
	  printf [
	      fmtInt(12, nbody), fmtReal(12, 5, dtime),
	      fmtReal(12, 4, eps), fmtReal(12, 2, tol),
	      fmtReal(12, 3, dtout), fmtReal(12, 2, tstop), "\n\n"
	    ];
	  case outfile
	   of "" => stopOutput()
	    | _ => outState := SOME{
		  dtime = dtime,
		  tout = tnow,
		  dtout = dtout,
		  strm = TextIO.openOut outfile
		}
	  (* end case *))

  (* compute set of dynamical diagnostics. *)
    fun diagnostics bodies = let
	  fun loop ([], arg) = {
		  mtot = #totM arg,		(* total mass *)
		  totKE = #totKE arg,		(* total kinetic energy *)
		  totPE = #totPE arg,		(* total potential energy *)
		  cOfMPos = #cOfMPos arg,	(* center of mass: position *)
		  cOfMVel = #cOfMVel arg,	(* center of mass: velocity *)
		  amVec = #amVec arg		(* angular momentum vector *)
		}
	    | loop (S.Body{
		  mass, pos=ref pos, vel=ref vel, acc=ref acc, phi=ref phi
	        } :: r, arg) = let
		val velsq = V.dotvp(vel, vel)
		val halfMass = 0.5 * mass
		val posXmass = V.mulvs(pos, mass)
		in
		  loop ( r, {
		      totM = (#totM arg) + mass,
		      totKE = (#totKE arg) + halfMass * velsq,
		      totPE = (#totPE arg) + halfMass * phi,
		      keTen = V.addm(#keTen arg, V.outvp(V.mulvs(vel, halfMass), vel)),
		      peTen = V.addm(#peTen arg, V.outvp(posXmass, acc)),
		      cOfMPos = V.addv(#cOfMPos arg, posXmass),
		      cOfMVel = V.addv(#cOfMVel arg, V.mulvs(vel, mass)),
		      amVec = V.addv(#amVec arg, V.mulvs(V.crossvp(pos, vel), mass))
		    })
		end
	  in
	    loop (bodies, {
		totM = 0.0, totKE = 0.0, totPE = 0.0,
		keTen = V.zerom, peTen = V.zerom,
		cOfMPos = V.zerov, cOfMVel = V.zerov,
		amVec = V.zerov
	      })
	  end (* diagnostics *)

    fun outputData (strm, tnow, nbody, bodies) = let
	  fun outInt i = fprintf(strm, ["  ", Int.toString i, "\n"])
	  fun outReal r = fprintf(strm, [" ", fmtRealE(21, 14, r), "\n"])
	  fun prReal r = fprintf(strm, [" ", fmtRealE(21, 14, r)])
	  fun outVec v = let
		fun out [] = TextIO.output(strm, "\n")
		  | out (x::r) = (prReal x; out r)
		in
		  out(V.explode v)
		end
	  in
	    outInt nbody;
	    outInt V.dim;
	    outReal tnow;
	    app (fn (S.Body{mass, ...}) => outReal mass) bodies;
	    app (fn (S.Body{pos, ...}) => outVec(!pos)) bodies;
	    app (fn (S.Body{vel, ...}) => outVec(!vel)) bodies;
	    printf ["\n\tparticle data written\n"]
	  end;

    fun output {nbody, bodies, n2bcalc, nbccalc, selfint, tnow} = let
	  val nttot = n2bcalc + nbccalc
	  val nbavg = floor(real n2bcalc / real nbody)
	  val ncavg = floor(real nbccalc / real nbody)
	  val data = diagnostics bodies
	  in
	    printf ["\n"];
	    printf (map (pad 9) [
		"tnow", "T+U", "T/U", "nttot", "nbavg", "ncavg", "selfint",
		"cputime"
	      ]);
	    printf ["\n"];
	    printf [
		fmtReal(9, 3, tnow), fmtReal(9, 4, #totKE data + #totPE data),
		fmtReal(9, 4, #totKE data / #totPE data), fmtInt(9, nttot),
		fmtInt(9, nbavg), fmtInt(9, ncavg), fmtInt(9, selfint),
		fmtReal(9, 2, cputime()), "\n\n"
	      ];
	    printvec ("cm pos", #cOfMPos data);
	    printvec ("cm vel", #cOfMVel data);
	    printvec ("am pos", #amVec data);
	    case !outState
	     of NONE => ()
	      | (SOME{tout, dtout, dtime, strm}) =>
		  if ((tout - 0.01 * dtime) <= tnow)
		    then (
		      outputData (strm, tnow, nbody, bodies);
		      outState := SOME{
			  tout=tout+dtout, dtout=dtout, dtime=dtime, strm=strm
			})
		    else ()
	    (* end case *)
	  end

  end; (* DataIO *)

