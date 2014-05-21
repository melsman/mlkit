(*************************************************************)
(* Driver                                                    *)
(*************************************************************)

structure Info : INFO = struct

fun memoize f =
    let val r = ref NONE
    in fn () => case !r of SOME v => v
			 | NONE => let val v = f()
				   in r:=SOME v; v
				   end
    end

val sysname =
    memoize (fn () =>
		case List.find (fn (f,_) => f = "sysname") (Posix.ProcEnv.uname()) of
		    SOME (_, name) => name
		  | _ => "unknown"
	    )

fun getInfo pid =
    case sysname() of
	"Linux" => LinuxInfo.getInfo pid
      | "Darwin" => NONE
      | _ => NONE
end
