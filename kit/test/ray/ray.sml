(* ray.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

structure Ray =
  struct
    local open Objects in

  (** basic operations on points and vectors **)

    fun scaleVector (s, VEC{l, m, n}) = VEC{l=s*l, m=s*m, n=s*n}

    fun vecPlusVec (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = VEC{l=l+l', m=m+m', n=n+n'}

    fun vecPlusPt (VEC{l, m, n}, PT{x, y, z}) = PT{x=x+l, y=y+m, z=z+n}

    fun ptMinusPt (PT{x, y, z}, PT{x=x', y=y', z=z'}) = VEC{l=x-x', m=y-y', n=z-z'}

    fun wave (PT{x, y, z}, PT{x=x', y=y', z=z'}, w) = PT{
	    x = w * (x' - x) + x,
	    y = w * (y' - y) + y,
	    z = w * (z' - z) + z
	  }

    fun dotProd (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = ((l*l') + (m*m') + (n*n'))

  (* normal vector to sphere *)
    fun normalSphere (Visible{h, s as Sphere{c, ...}}) = let
	  val n = ptMinusPt(h, c)
	  val norm = Math.sqrt(dotProd(n, n))
	  in
	    scaleVector(1.0 / norm, n)
	  end

  (* intersect a ray with a sphere *)
    fun intersectSphere (Ray ray, s as Sphere sphere) = let
	  val a = dotProd(#d ray, #d ray)
	  val sdiffc = ptMinusPt(#s ray, #c sphere)
	  val b = 2.0 * dotProd(sdiffc, #d ray)
	  val c = dotProd(sdiffc, sdiffc) - (#r sphere * #r sphere)
	  val d = b*b - 4.0*a*c
	  in
	    if (d <= 0.0)
	      then Miss
	      else let
		val d = Math.sqrt(d)
		val t1 = (~b - d) / (2.0 * a)
		val t2 = (~b + d) / (2.0 * a)
		val t = if ((t1 > 0.0) andalso (t1 < t2)) then t1 else t2
		in
		  Hit{t=t, s=s}
		end
	  end

  (* simple shading function *)
    fun shade {light, phi} (visible as Visible{h, s}) = let
	  val l = ptMinusPt(light, h)
	  val n = normalSphere(visible)
	  val irradiance = phi * dotProd(l,n) / dotProd(l,l);
	  val irradiance = (if (irradiance < 0.0) then 0.0 else irradiance) + 0.05
	  val Sphere{color=Color{red, grn, blu}, ...} = s
	  in
	    Color{red=red*irradiance, grn=grn*irradiance, blu=blu*irradiance}
	  end

    fun trace (ray as (Ray ray'), objList) = let
	  fun closest (Miss, x) = x
	    | closest (x, Miss) = x
	    | closest (h1 as Hit{t=t1, ...}, h2 as Hit{t=t2, ...}) =
		if (t2 < t1) then h2 else h1
	  fun lp ([], Hit{t, s}) = Visible{
		  h = vecPlusPt(scaleVector(t, #d ray'), #s ray'),
		  s = s
		}
	    | lp (s :: r, closestHit) =
		lp (r, closest (closestHit, intersectSphere (ray, s)))
	    | lp _ = raise Fail "trace"
	  in
	    lp (objList, Miss)
	  end

    fun camera (Camera cam) (x, y) = let
	  val l = wave (#ul cam, #ll cam, y)
	  val r = wave (#ur cam, #lr cam, y)
	  val image_point = wave(l, r, x)
	  in
	    Ray{d = ptMinusPt(image_point, #vp cam), s = #vp cam}
	  end

    val shade = shade {light = PT{x = 10.0, y = ~10.0, z = ~10.0}, phi = 16.0}
    val camera = camera (Camera{
	    vp = PT{x = 0.0, y = 0.0, z = ~3.0},
	    ul = PT{x = ~1.0, y = ~1.0, z = 0.0},
	    ur = PT{x = 1.0, y = ~1.0, z = 0.0},
	    ll = PT{x = ~1.0, y = 1.0, z = 0.0},
	    lr = PT{x = 1.0, y = 1.0, z = 0.0}
	  })

    fun image objList (x, y) = shade (trace(camera(x, y), objList))

    fun picture (picName, objList) = let
	  val outStrm = TextIO.openOut picName
	  val image = image objList
	  val print = fn x => TextIO.output (outStrm, x)
	  fun putc c = TextIO.output1(outStrm, chr c)
	  fun doPixel (i, j) = let
		val x = (real i) / 512.0
		val y = (real j) / 512.0
		val (Color c) = image (x, y)
		fun cvt x = if (x >= 1.0) then 255 else floor(256.0*x)
		in
		  putc (cvt (#red c));
		  putc (cvt (#grn c));
		  putc (cvt (#blu c))
		end
	  fun lp_j j = if (j < 512)
		then let
		  fun lp_i i = if (i < 512)
			then (doPixel(i, j); lp_i(i+1))
			else ()
		  in
		    lp_i 0; lp_j(j+1)
		  end
		else ()
	  in
	    print "TYPE=dump\n";
	    print "WINDOW=0 0 512 512\n";
	    print "NCHAN=3\n";
	    print "CHAN=rgb\n";
	    print "\n";
	    lp_j 0;
	    TextIO.closeOut outStrm
	  end

    end (* local *)
  end; (* Ray *)
