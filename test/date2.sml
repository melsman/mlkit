
val d = Date.day (Date.date {year=2014,month=Date.Nov,day=12,hour=0,
                             minute=0,second=0,offset=NONE})

val () = if d = 12 then print "Ok\n" else print "Error\n"
