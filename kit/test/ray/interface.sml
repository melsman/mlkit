(* interface.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * The interface between the interpreter and the ray tracer.
 *)

structure Interface =
  struct
    local
      open Objects
    in

  (* color pops three numbers and pushes a color object.
   * usage: red-value green-value blue-value color
   *)
    fun ps_color ((NUMBER blu)::(NUMBER grn)::(NUMBER red)::r) =
	  (COLOR(Color{red=red, grn=grn, blu=blu})) :: r
      | ps_color stk = Interp.error "color" stk

  (* pop radius, coordinates of center, and a color and push a sphere
   * usage: radius x y z color-value sphere
   *)
    fun ps_sphere (
	  (COLOR c)::(NUMBER z)::(NUMBER y)::(NUMBER x)::(NUMBER rad)::r
	) = SPHERE(Sphere{c=PT{x=x, y=y, z=z}, r=rad, color=c}) :: r
      | ps_sphere stk = Interp.error "sphere" stk

  (* build an object list from solids on the stack, then invoke raytracer *)
    fun ps_raytrace ((LITERAL picName)::r) = let
	  fun mkObjList ([], l) = l
	    | mkObjList ((SPHERE s)::r, l) = mkObjList(r, s::l)
	    | mkObjList (_::r, l) = mkObjList(r, l)
	  in
	    Ray.picture(picName, mkObjList(r, []));
	    []
	  end
      | ps_raytrace stk = Interp.error "raytrace" stk

  (* add ray tracing operations to interpreter dictionary *)
    fun rtInit () = (
	  Interp.installOperator("color", ps_color);
	  Interp.installOperator("sphere", ps_sphere);
	  Interp.installOperator("raytrace", ps_raytrace))

    end (* local *)
  end;

