(* objects.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Type declarations for the various objects in the ray tracer.
 *)

structure Objects =
  struct

    datatype point = PT of {x : real, y : real, z : real}

    datatype vector = VEC of {l : real, m : real, n : real}

    datatype ray = Ray of {s : point, d : vector}

    datatype camera = Camera of {
	vp : point,
	ul : point,
	ur : point,
	ll : point,
	lr : point
      }

    datatype color = Color of {red : real, grn : real, blu : real}

    datatype sphere = Sphere of {c : point, r : real, color : color}

    datatype hit = Miss | Hit of {t : real, s : sphere}

    datatype visible = Visible of {h : point, s : sphere}

    datatype object
      = TOP
      | NUMBER of real
      | NAME of string
      | LIST of object list
      | OPERATOR of object list -> object list
      | MARK
      | LITERAL of string
      | UNMARK
      | POINT of point
      | VECTOR of vector
      | RAY of ray
      | CAMERA of camera
      | COLOR of color
      | SPHERE of sphere
      | HIT
      | VISIBLE

  end (* Objects *)
