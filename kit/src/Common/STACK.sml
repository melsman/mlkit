(*$STACK:*)
signature STACK =
  sig
    type '_a stack

    exception EmptyStack
    val empty : unit -> '_a stack
    val push : '_a stack * '_a -> unit
    val peek : '_a stack -> '_a
    val pop : '_a stack -> '_a
    val drop : '_a stack -> unit
    val clear : '_a stack -> unit
  end;
  
