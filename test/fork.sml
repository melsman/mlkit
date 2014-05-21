val _ = case Posix.Process.fork() of
    SOME i => print "In Parrent\n"
  | NONE => print "In Child\n"