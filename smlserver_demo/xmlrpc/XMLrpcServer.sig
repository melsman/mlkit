signature XMLrpcServer = sig

    exception TypeConversion

    type 'a pu

    (* XMLRPC type <int> and <i4> *)
    val int : int pu

    (* XMLRPC type <boolean> *)
    val bool : bool pu
    
    (* XMLRPC type <double> *)
    val real : real pu

    (* XMLRPC type <string> *)
    val string : string pu

    (* XMLRPC type <dateTIme.iso8601> *)
    val	date : Date.date pu

    (* XMLRPC type <struct> where type 'a = member with name=1 and 'b = menber with name=2 *)
    val pair : 'a pu * 'b pu -> ('a * 'b) pu

    (* XMLRPC type <array>. In this implementation the array is only allowed to contain values of same type *)
    val list : 'a pu -> 'a list pu

    (* XMLRPC type <array> *)
    val array : 'a pu -> 'a Array.array pu

    (* XMLRPC type <array> *)
    val vector : 'a pu -> 'a Vector.vector pu

    val export : 'a pu -> 'b pu -> ('a -> 'b) -> string -> string
      
end
