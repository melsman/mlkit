fun add (a:int,b:int) : int = a + b
fun neg (a:int) : int = ~a

local open Web.XMLrpc
in (* val _ = Web.log(Web.Notice, "in xmlrpc_test_server.sml") *)
   val _ =
     dispatch [method "add" (pair(int,int)) int add,
	       method "neg" int int neg]
end

