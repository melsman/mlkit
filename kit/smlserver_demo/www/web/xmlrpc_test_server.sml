fun add (a:int,b:int) : int = a + b

fun neg (a:int) : int = ~a

fun toInt s = case Int.fromString s of SOME i => i
                                     | NONE => raise Fail "parseInt"

fun guests (n) : ((int * string) * (string*string)) list =
    Db.fold (fn (f,a) => ((toInt(f "gid"),f "name"), (f "email", f "comments"))::a) nil
    `select gid,email,name,comments
     from guest
     order by name`

fun guest_del(gid:int) : bool =
    let val () = Db.dml `delete from guest where gid = ^(Int.toString gid)`
    in true
    end handle _ => false

local open Web.XMLrpc
in (* val _ = Web.log(Web.Notice, "in xmlrpc_test_server.sml") *)
   val _ =
     dispatch [method "add" (pair(int,int)) int add,
	       method "neg" int int neg,
               method "guests" int (list(pair(pair(int,string),pair(string,string)))) guests,
               method "guest_del" int bool guest_del]
end

