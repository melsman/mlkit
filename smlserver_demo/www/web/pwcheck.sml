(* uses cracklib2 deb package *)

fun isNullFP (x : foreignptr) = prim("__is_null",x) : bool

fun mylog (Fail x) = (Web.log(Web.Notice, x) ; raise Fail x)
  | mylog e = raise e

val b = Web.WebDynlib.dlopen (SOME "libcrack.so", Web.WebDynlib.NOW, false)
          handle Fail x => mylog (Fail x)
val a = Web.WebDynlib.dlsym ("testdyn1", "FascistCheck", b)
          handle Fail x => mylog (Fail x)

fun fascistCheck a : string option = 
             let val b : foreignptr = prim("@:", ("testdyn1", a : string, "/usr/lib/cracklib_dict"))
             in if isNullFP b then NONE else SOME(prim ("fromCtoMLstring", b))
             end
structure FV = FormVar
val input = FV.wrapOpt FV.getStringErr "password"

val data = Quot.fromString (
         case input of NONE => ""
                     | SOME pw => let val r = fascistCheck pw
                                  in
                                  case r of NONE => "PassWord OK" 
                                          | SOME m =>  "Bad PassWord: " ^ m
                                  end)

val _ = 
   Page.return "Password checking" (
   `
    Enter a password:
    <form method=post action=pwcheck.sml>
      <input type=text name=password>
      <input type=submit value=Check Password>
    </form>` ^^ data)
   
