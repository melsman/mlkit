structure Info : INFO =
struct

    val getInfo: string -> {size:int, rss:int} = fn _ => {size=0,rss=0}

end (* signature INFO *)