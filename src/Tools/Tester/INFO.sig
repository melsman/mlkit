signature INFO =
sig

  val getInfo: string -> 
    {size:int, rss:int, data:int,
     stk:int, exe:int} option (* bytes *)

end (* signature INFO *)
