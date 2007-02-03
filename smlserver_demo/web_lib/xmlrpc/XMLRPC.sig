signature XMLRPC = sig
    include XMLRPC_TYPE
    
    exception TypeConversion
    exception MethodInvocation of (int * string)
    exception ServerConnection of string

    val rpc : 'a T -> 'b T -> {url : string, method : string} 
	      -> ('a -> 'b)
	      
    type method
    val dispatch : method list -> unit
    val method : string -> 'a T -> 'b T -> ('a -> 'b) -> method
end

(*
 [TypeConversion] is raised whenever a recieved value dosen't match 
 the expected type.

 [MethodInvocation (code, str)] is raised when a fault message is
 recieved from the server; code is the error code returned and str the
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

 [type method] type of method.

 [method m A B f] returns a method of name m bound to the function f
 of type A -> B.

 [dispatch ms] executes the first method in the list ms for which the
 name equals the extracted actual method name from the client
 request. Raises exception ServerConnection in case of connection
 errors.  
*)
