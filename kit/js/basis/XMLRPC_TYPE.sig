signature XMLRPC_TYPE = sig
    type 'a T
    val int    : int T
    val bool   : bool T
    val real   : real T
    val string : string T
    val	date   : Date.date T
    val unit   : unit T
    val pair   : 'a T * 'b T -> ('a * 'b) T
    val tup3   : 'a T * 'b T * 'c T -> ('a * 'b * 'c) T
    val tup4   : 'a T * 'b T * 'c T * 'd T -> ('a * 'b * 'c * 'd) T
    val option : 'a T -> 'a option T
    val list   : 'a T -> 'a list T
    val array  : 'a T -> 'a Array.array T
    val vector : 'a T -> 'a Vector.vector T
    val conv   : ('a -> 'b) * ('b -> 'a) -> 'b T -> 'a T
end

(*
 [int] type <int> and <i4>

 [bool] type <boolean>

 [real] type <double>

 [string] type <string>

 [date] type <dateTime.iso8601>

 [unit] type <int> with value 0.

 [pair] type <array> where type 'a is type of member with index 0 and
 'b is type of member with index 1.

 [tup3] type <array> where type 'a is type of member with index 0, 'b 
 is type of member with index 1, and 'c is type of member with index 2.

 [tup4] type <array> where type 'a is type of member with index 0, 'b 
 is type of member with index 1, 'c is type of member with index 2,
 and 'd is type of member with index 3.

 [option] type <array> with either one element or zero elements.

 [list] type <array>. Homogeneous lists where elements must be of
 identical type.

 [array] type <array>. Homogeneous arrays where elements must be of
 identical type.

 [vector] type <array>. Homogeneous vectors where elements must be of
 identical type.

 [option] type <array> with zero or one element.

 [conv (f,g) a] allows for constructing a serializer of arbitrary
 type, given an empedding-projection pair (f,g) into the type a. 
*)
