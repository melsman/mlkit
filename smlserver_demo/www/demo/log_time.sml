val time_of_day = 
  Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))

val _ = Ns.log(Ns.Notice, "Script log_time.sml; time of day: " ^time_of_day)

