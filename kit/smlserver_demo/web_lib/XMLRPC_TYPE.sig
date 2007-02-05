signature XMLRPC_TYPE = sig
    type 'a T
    val int    : int T
    val bool   : bool T
    val real   : real T
    val string : string T
    val	date   : Date.date T
    val pair   : 'a T * 'b T -> ('a * 'b) T
    val list   : 'a T -> 'a list T
    val array  : 'a T -> 'a Array.array T
    val vector : 'a T -> 'a Vector.vector T
end

(*
 [int] type <int> and <i4>

 [bool] type <boolean>

 [real] type <double>

 [string] type <string>

 [date] type <dateTIme.iso8601>

 [pair] type <struct> where type 'a = member with name=1 and 'b =
 member with name=2

 [list] type <array>. Homogeneous lists where elements must be of
 identical type.

 [array] type <array>. Homogeneous arrays where elements must be of
 identical type.

 [vector] type <array>. Homogeneous vectors where elements must be of
 identical type.
*) 
