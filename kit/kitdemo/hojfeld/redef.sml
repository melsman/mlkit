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

