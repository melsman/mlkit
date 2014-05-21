(** XMLRPC support.

See http://www.xmlrpc.com 
*)

signature XMLRPC = sig
    include XMLRPC_TYPE
    
    exception TypeConversion
    exception MethodInvocation of (int * string)
    exception ServerConnection of string

    val rpc : 'a T -> 'b T -> {url : string, method : string} 
	      -> 'a -> 'b

    val rpcAsync : 'a T -> 'b T -> {url : string, method : string} 
	      -> 'a -> ('b -> unit) -> unit
end

(**
[TypeConversion] is raised whenever a recieved value dosen't match 
the expected type.

[MethodInvocation (code, str)] is raised when a fault message is
received from the server; code is the error code returned and str the
fault string returned.
  
[ServerConnection] is raised if problems occur during reading or
writing to the connection.

[rpc A B {url, method}] returns a function of type (A -> B) that when
called will connect to the XML-RPC server resident at the address
specified by url. The function will call the specified method on the
server.
  
The returned function raises: 

   - TypeConversion if the returned XML-RPC response cannot be
     converted to a value of type B.

   - MethodInvocation if a fault value is returned from the server

   - ServerConnection if problems occur during reading or writing
     to the connection.

[rpcAsync A B {url, method} a f] initiates an asyncronous method call
to method on the XML-RPC server resident at the address specified by
url. The argument a is passed to the method and the function f is
called once a result (of type B) is obtained from the server.
*)
