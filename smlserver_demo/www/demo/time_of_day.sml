  val time_of_day = 
    Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))

  val _ = Ns.Conn.return 
    ("<html> \
     \  <head><title>Time-of-day</title></head> \
     \  <body bgcolor=white> \
     \    <h2>Time-of-day</h2> \
     \       The time-of-day is " ^ time_of_day ^ ". \
     \    <hr> <i>Served by \
     \    <a href=http://www.smlserver.org>SMLserver</a> \
     \    </i> \
     \  </body> \
     \</html>")
