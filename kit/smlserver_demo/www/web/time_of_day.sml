  val time_of_day = 
    Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))

    val _ = Page.return "Time of day" (`
       <body bgcolor=white> 
            The time of day is `  ^^ Quot.fromString time_of_day ^^ `.`
            )
