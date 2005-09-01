val time_of_day = 
  Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))

val _ = Web.log(Web.Notice, "Script log_time.sml; time of day: " ^time_of_day)

