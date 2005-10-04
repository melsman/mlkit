structure TextIO =
struct
  open TextIO
  val inputLine = fn x =>
     case inputLine x of 
	 "" => NONE
       | s => SOME s
end

structure Timer =
struct
    open Timer
    val checkCPUTimer = fn x =>
      let val {sys,usr,gc} = checkCPUTimer x
      in {sys=sys,usr=Time.+(usr,gc)}
      end
end

local
    structure NewPath =
	struct
	    open OS.Path
	    val mkRelative = fn {path,relativeTo} => mkRelative(path,relativeTo)
	    val mkAbsolute = fn {path,relativeTo} => mkAbsolute(path,relativeTo)
	end
in
    structure OS =
	struct
	    open OS
	    structure Path = NewPath
	end
end

structure CharVector =
    struct
	open CharVector
	val foldli = fn f => fn b => fn s => foldli f b (s,0,NONE)
    end