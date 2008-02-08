signature XMLRPC_TYPE = sig
    type 'a T
(*    val unit   : unit T *)
    val int    : int T
    val bool   : bool T
    val real   : real T
    val string : string T
    val	date   : Date.date T
    val pair   : 'a T * 'b T -> ('a * 'b) T
(*
    val tup3   : 'a T * 'b T * 'c T -> ('a * 'b * 'c) T
    val tup4   : 'a T * 'b T * 'c T * 'd T -> ('a * 'b * 'c * 'd) T
    val tup5   : 'a T * 'b T * 'c T * 'd T * 'e T -> ('a * 'b * 'c * 'd * 'e) T
    val option : 'a T -> 'a option T
*)
    val list   : 'a T -> 'a list T
    val array  : 'a T -> 'a Array.array T
    val vector : 'a T -> 'a Vector.vector T
end

(*
 [unit] type <int> with value 0.

 [int] type <int> and <i4>

 [bool] type <boolean>

 [real] type <double>

 [string] type <string>

 [date] type <dateTIme.iso8601>

 [pair] type <struct> where type 'a = member with name=1 and 'b =
 member with name=2

 [tup3] type <struct> where type 'a = member with name=1, 'b =
 member with name=2, and 'c = member with name=3

 [tup4] type <struct> where type 'a = member with name=1, 'b =
 member with name=2, 'c = member with name=3, and 'd = member with 
 name=4

 [tup5] type <struct> where type 'a = member with name=1, 'b =
 member with name=2, 'c = member with name=3, 'd = member with 
 name=4, and 'e = mamber with name=5

 [list] type <array>. Homogeneous lists where elements must be of
 identical type.

 [array] type <array>. Homogeneous arrays where elements must be of
 identical type.

 [vector] type <array>. Homogeneous vectors where elements must be of
 identical type.

 [option] type <array> with zero or one element.
*) 
