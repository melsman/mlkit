  val time_of_day = 
    Date.fmt "%H.%M" (Date.fromTimeLocal(Time.now()))
  val _ = Ns.Conn.return 
    ("<html> \
     \  <head><title>Time-of-day</title></head> \
     \  <body bgcolor=white> \
     \    <h2>Time-of-day</h2> \
     \       The time-of-day is " ^ time_of_day ^ ". \
     \    <hr> \
     \    <a href=\"mailto: mael@it.edu\">mael@it.edu</a> \
     \  </body> \
     \</html>")
