(*redef.sml  23/06/1997 16:00. tho.*)
(*In sml '97 it is no longer legal to redefine true, nil, etc.*)
(*
datatype t = true of int (*illegal*) | False;

  gives

  datatype t = true of int (*illegal*) | False;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
datatype t = true | False
exception false of int

gives

  datatype t = true | False
               ^^^^^^^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.

  exception false of int
            ^^^^^^^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
exception :: = Bind

gives

  exception :: = Bind
            ^^^^^^^^^
unbound identifier Bind.
*)
(*
exception E of int
exception :: = E

gives

  exception :: = E
            ^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
val nil = []

is ok!  But:

  datatype t = nil
               ^^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
  datatype 'a t = Ref | REF |ref
                             ^^^
You may not rebind `true', `false', `nil', `::', or `ref'.

and

  datatype 'a t = Ref | REF | ref | RAF

gives:

  datatype 'a t = Ref | REF | ref | RAF
                              ^^^^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
    datatype 'a t = Ref | REF | ref | RAF | ::
                                            ^^
You may not rebind `true', `false', `nil', `::', or `ref'.
*)
(*
  datatype 'a t = Ref | REF | ref | RAF | :: | ref

gives a whole bunch of errors:

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 74, column 30:
    datatype 'a t = Ref | REF | ref | RAF | :: | ref
                                ^^^^^^^^^^^^^^^^^^^^
Repeated identifier ref.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 74, column 42:
    datatype 'a t = Ref | REF | ref | RAF | :: | ref
                                            ^^^^^^^^
You may not rebind `true', `false', `nil', `::', or `ref'.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 74, column 47:
    datatype 'a t = Ref | REF | ref | RAF | :: | ref
                                                 ^^^
You may not rebind `true', `false', `nil', `::', or `ref'.

cute.
*)
(*
signature s = sig
		exception it of bool
	      end

gives

                  exception it of bool
                            ^^^^^^^^^^
You may not specify `it' as a constructor.
*)
(*
signature s = sig
		exception it of bool
		exception it of bool
	      end

	    gives

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 110, column 16:
                  exception it of bool
                  ^^^^^^^^^^^^^^^^^^^^
                  exception it of bool
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Repeated identifier it.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 110, column 26:
                  exception it of bool
                            ^^^^^^^^^^
You may not specify `it' as a constructor.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 111, column 26:
                  exception it of bool
                            ^^^^^^^^^^
You may not specify `it' as a constructor.


*)
(*
signature s = sig
		val t : bool
		datatype it (*legal*) = true | FALSE
	      end

gives

                  datatype it (*legal*) = true | FALSE
                                          ^^^^^^^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.
*)
(*
signature s = sig
		val t : bool
		datatype it (*legal*) = true | it
	      end
	    

gives

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 153, column 40:
                  datatype it (*legal*) = true | it
                                          ^^^^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 153, column 47:
                  datatype it (*legal*) = true | it
                                                 ^^
You may not specify `it' as a constructor.
*)
(*
signature s = sig
		val t : bool
		val true : bool
	      end

	    gives

                  val true : bool
                      ^^^^^^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.
*)
(*
signature s = sig
		type t
		val :: : t
	      end

	   gives

                  val :: : t
                      ^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.
*)
(*
signature s = sig
		type ref
		type t
		val ref : t
	      end

gives

                  val ref : t
                      ^^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.
*)
(*
signature s = sig
		exception false
		type t
		val nil : t
	      end

gives

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 208, column 26:
                  exception false
                            ^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.


/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/redef.sml, line 210, column 20:
                  val nil : t
                      ^^^^^^^
You may not specify `true', `false', `nil', `::', or `ref'.
*)

