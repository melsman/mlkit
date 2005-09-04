val _ = Web.log (Web.Notice, "trap.sml: " ^ Web.Info.pageRoot() ^ Web.Conn.url())
val _ = Web.returnFile (Web.Info.pageRoot() ^ Web.Conn.url())
