(* XMLRPCCLIENT - a XML-RPC client  
   Author: Martin Olsen
   Version 1.0
*)
signature XMLRPCCLIENT = sig 

    exception TypeConversion

    exception MethodInvocation of (int * string)

    exception ServerConnection of string
	      
    type 'a pu
     
    val rpc : 'a pu -> 'b pu -> {url : string, method : string} -> ('a -> 'b)

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

    (* XMLRPC type <struct> where type 'a = member with name=1 
       and 'b = member with name=2 *)
    val pair : 'a pu * 'b pu -> ('a * 'b) pu

    (* XMLRPC type <array>. In this implementation the array is only allowed to contain values of same type *)
    val list : 'a pu -> 'a list pu

    (* XMLRPC type <array> *)
    val array : 'a pu -> 'a Array.array pu

    (* XMLRPC type <array> *)
    val vector : 'a pu -> 'a Vector.vector pu
	  
end

(*

 [TypeConversion] is raised whenever a recieved type dosen't match 
  the expected type.

 [MethodInvocation (code, str)] is raised when a fault message is 
  recieved from the server. code is the error code returned and 
  str the fault string returned.
  
 [ServerConnection] is raised if problems during reading or 
  writing to the connection occurs

 [rpc 'a pu 'b pu {url, method}] returns a function 
  ('a -> 'b) that when called will connect to the XML-RPC 
  server resident at the address specified by url. The function 
  will call the specified method on the server.   
  When doing the remote procedure call the 'a pickler will 
  be used to pickle from the argument 'a to the corresponding 
  XMLRPC type. 
  When the remote procedure call returns with a 
  valid value the 'b unpickler will be used to convert the 
  XMLRPC type to the type of 'b. 
  The returned function raises: 
   - TypeConversion if the returned XMLRPC type cannot 
     be converted to type 'b.
   - MethodInvocation if a fault value is returned
     from the server
   - ServerConnection if problems during read or write 
     to the connection occurs.

 [int] Pickler for ml type int to XMLRPC <int> and unpickler from 
  XMLRPC type <int> or <i4> to ml type int 

 [bool] Pickler for ml type bool to XMLRPC type <boolean> and 
  unpickler from XMLRPC type <boolean> to ml type bool.

 [real] Pickler for ml type real to XMLRPC type <double> and 
  unpickler from XMLRPC type <double> to ml type real.

 [string] Pickler for ml type string to XMLRPC type <string> and 
  unpickler from XMLRPC type <string> to ml type string.

 [date] Pickler for ml type Date.date to XMLRPC type <dateTime.iso8601>
  and unpickler from XMLRPC type <dateTime.iso8601> to ml type Date.date.

 [pair (a' pu, 'b pu)] Pickler and unpickler for ml type ('a, 'b) 
  to XMLRPC type <struct>. The pickler will create a XMLRPC struct 
  with two members where 'a has <name> = 1 and 'b <name> = 2. The a' and 
  'b  picklers will be used to create the values for the two members. 
  The unpickler expects a XMLRPC struct with two members. If the names of
  these members is two integers in preceding order the unpickler will 
  unpickle the struct using 'a unpickler on the member with the name with 
  the smallest value and 'b unpickler on the other. Otherwise 'a unpickler 
  will be used to unpickle the first member appearing in the struct and 
  'b unpickler to unpickle the second member. 

 [list 'a pu] Pickler for ml type 'a list to XMLRPC type <array> and unpickler
  from XMLRPC type <array> to ml type 'a list. The unpickler expects all values
  in the recieved array to be of type 'a.

 [vector 'a pu] Pickler for ml type 'a Vector.vector to XMLRPC type <array> and unpickler
  from XMLRPC type <array> to ml type 'a Vector.vector. 

 [array 'a pu] Pickler for ml type 'a Array.array to XMLRPC type <array> and unpickler
  from XMLRPC type <array> to ml type 'a Array.array. 

*)