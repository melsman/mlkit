signature SCS_DATE =
  sig
    exception ScsDate of string
    val all_weekdays : Date.weekday list

    type day = int
    type mth = int
    type year = int
    type hour = int
    type min = int
    type sec = int

    val mthToName : mth -> Date.month
    val mthFromName : Date.month -> mth

    val genDate      : day * mth * year -> Date.date
    val genTimestamp : day * mth * year * hour * min * sec -> Date.date

    val now_local : unit -> Date.date
    val now_univ  : unit -> Date.date

    (* Calculating with minutes and hours *)
    type hour_min = 
      {negative_p : bool,
       minutes    : int, (* Always positive *)
       hours      : int  (* Always positive *)}

    val genHourMin   : min * hour -> hour_min
    val hourMinToMin : hour_min -> min
    val minToHourMin : min -> hour_min
    val normHourMin  : hour_min -> hour_min
    val ppHourMin'   : hour_min -> string
    val hourMinSub   : hour_min * hour_min -> hour_min
    val hourMinAdd   : hour_min * hour_min -> hour_min
    val hourMinCmp   : hour_min * hour_min -> General.order

    (* Misc Operations *)
    val leap : year -> bool
    val preceedingLeaps : year -> int
    val daysInMonth : year -> mth -> int
    val dateOk : day * mth * year -> bool
    val timeOk : hour * min * sec -> bool
    val preceedingDays : day * mth * year -> int
    val dateInPeriod      : Date.date * Date.date * Date.date -> bool
    val nearstDayInPeriod : Date.date * Date.date * Date.date -> Date.date
    val currDateInPeriod : Date.date * Date.date -> bool
    val yearsInPeriod          : Date.date * Date.date -> int list
    val firstMthInYearInPeriod : int * Date.date * Date.date -> int
    val lastMthInYearInPeriod  : int * Date.date * Date.date -> int
    val allMthsInYearInPeriod  : int * Date.date * Date.date -> int list
    val half_year        : Date.date -> Date.date * Date.date
    val semester         : Date.date -> Date.date * Date.date
    val add_secs         : Date.date -> int -> Date.date
    val add_days         : Date.date -> int -> Date.date
    val getWeekNo        : Date.date -> int     (* Monday as first day of week *)
    val getDayOfWeek     : Date.date -> int (* Monday = 0, ..., Sunday = 6 *)
    val firstDateInWeek  : Date.date -> Date.date
    val lastDateInWeek   : Date.date -> Date.date
    val getDayOfMonth    : Date.date -> int (* 0 -- 31 *)
    val firstDateInMonth : Date.date -> Date.date
    val lastDateInMonth  : Date.date -> Date.date
    val min : Date.date * Date.date -> Date.date
    val max : Date.date * Date.date -> Date.date
    val sameDayPrevMth : Date.date -> Date.date
    val sameDayNextMth : Date.date -> Date.date

    (* [dateCmp (d1,d2)] similar to Date.compare except that this one
        only takes day,month and year into account. *)
    val dateCmp : Date.date * Date.date -> General.order

    (* PrettyPrinting *)
    val wrapOpt : ('a -> string) -> 'a option -> string
    val ppIso : Date.date -> string
    val ppDk  : Date.date -> string
    val ppLongDk  : Date.date -> string
    val ppLongEng : Date.date -> string
    val ppWeekdayDk : Date.weekday -> string
    val ppWeekdayEng : Date.weekday -> string
    val ppWeekday          : Date.weekday -> string
    val ppWeekdayOneLetter : Date.weekday -> string
    val ppDay : Date.date -> string
    val ppMth : Date.month -> string
    val ppMthDk : Date.month -> string
    val ppMthEng : Date.month -> string
    val ppMthFromDate : Date.date -> string
    val ppHourMin : Date.date -> string
    val ppLong               : Date.date -> string 
    val ppLongWithWeekdayDk  : Date.date -> string 
    val ppLongWithWeekdayEng : Date.date -> string 
    val ppLongWithWeekday    : Date.date -> string 
    val pp    : Date.date -> string
    val ppDb  : Date.date option -> string
    val ppDbExamples : Date.date option -> string
    val ppTimestamp : Date.date -> string
    val ppTimestampDb : Date.date option -> string

    val weekday_from_DB : string -> Date.weekday option
    val weekday_to_DB   : Date.weekday -> string

    (* Widgets *)

  end

structure ScsDate :> SCS_DATE =
  struct
    val all_weekdays = [
      Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun 
    ]


    exception ScsDate of string
    type day = int
    type mth = int
    type year = int
    type hour = int
    type min = int
    type sec = int

    fun mthToName mth =
      case mth of
	1 => Date.Jan
      | 2 => Date.Feb
      | 3 => Date.Mar
      | 4 => Date.Apr
      | 5 => Date.May
      | 6 => Date.Jun
      | 7 => Date.Jul
      | 8 => Date.Aug
      | 9 => Date.Sep
      | 10 => Date.Oct
      | 11 => Date.Nov
      | 12 => Date.Dec
      | _ => raise ScsDate ("Wrong month: " ^ (Int.toString mth))

    fun mthFromName mth =
      case mth of
	Date.Jan => 1
      | Date.Feb => 2
      | Date.Mar => 3
      | Date.Apr => 4
      | Date.May => 5
      | Date.Jun => 6
      | Date.Jul => 7
      | Date.Aug => 8
      | Date.Sep => 9
      | Date.Oct => 10
      | Date.Nov => 11
      | Date.Dec => 12

    fun now_local () = Date.fromTimeLocal(Time.now())
    fun now_univ () = Date.fromTimeUniv(Time.now())

    fun genDate (d,m,y) =
      Date.date{year=y,month=mthToName m,day=d,hour=0,minute=0,second=0,offset=NONE}
      handle Date.Date => raise ScsDate ("Wrong date: " ^ (Int.toString y) ^ "-" ^ 
					 (Int.toString m) ^ "-" ^ (Int.toString d))

    fun genTimestamp (day,mth,year,hour,minute,sec) =
      Date.date{year=year,month=mthToName mth,day=day,hour=hour,minute=minute,second=sec,offset=NONE}
      handle Date.Date => 
	raise ScsDate ("Wrong timestamp: " ^ (Int.toString year) ^ "-" ^ (Int.toString mth) ^ "-" ^ (Int.toString day)
		       ^ (Int.toString hour) ^ ":" ^ (Int.toString minute) ^ "." ^ (Int.toString sec))

    (* Calculating with minutes and hours *)
    type hour_min = 
      {negative_p : bool,
       minutes    : int, (* Always positive *)
       hours      : int  (* Always positive *)}

    fun normHourMin ({minutes, hours,...} : hour_min) = 
      let
	val sum = hours*60+minutes
	val neg_p = if sum < 0 then true else false
	val sum = Int.abs(sum)
      in
	{negative_p = neg_p, minutes = Int.mod(sum,60), hours = Int.div(sum,60)}
      end

    fun genHourMin (min: int, hours: int) = 
      normHourMin {minutes=min,hours=hours,negative_p = false (*arbitrary!*)}

    fun ppHourMin' ({negative_p, minutes, hours} : hour_min) = 
      let
	val neg = if negative_p then "-" else ""
	val h = StringCvt.padLeft #"0" 2 (Int.toString hours)
	val m = StringCvt.padLeft #"0" 2 (Int.toString minutes)
      in
	Quot.toString `^(neg)^(h):^(m)`
      end

    fun hourMinToMin ({negative_p,minutes,hours}:hour_min) =
      (if negative_p then ~1 else 1) * (60*hours + minutes)

    fun minToHourMin (min:int) = genHourMin(min,0)

    fun hourMinSub (hm1:hour_min,hm2:hour_min) = 
      minToHourMin(hourMinToMin hm1 - (hourMinToMin hm2))

    fun hourMinAdd (hm1:hour_min,hm2:hour_min) = 
      minToHourMin(hourMinToMin hm1 + (hourMinToMin hm2))

    fun hourMinCmp (hm1:hour_min,hm2:hour_min) =
      if hourMinToMin hm1 < hourMinToMin hm2 then
	General.LESS
      else if hourMinToMin hm2 > hourMinToMin hm2 then
	General.GREATER
	   else
	     General.EQUAL

    fun leap y = (Int.mod(y,4) = 0 andalso Int.mod(y,100) <> 0) orelse Int.mod(y,400) = 0

    (* Return the number of leap years since year 0 and before year y *)
    fun preceedingLeaps y =
      if y = 0 then 
	0 
      else 
	let
	  val y = y - 1
	in
	  Int.div(1 + y,4) - Int.div(y,100) + Int.div(y,400)
	end

    (* Return the number of days in a given month *)
    fun daysInMonth y m =
      case m of
	1 => 31
      | 2 => if leap y then 29 else 28
      | 3 => 31
      | 4 => 30
      | 5 => 31
      | 6 => 30
      | 7 => 31
      | 8 => 31
      | 9 => 30
      | 10 => 31
      | 11 => 30
      | 12 => 31
      | _ => raise ScsDate ("Month " ^ (Int.toString m) ^ " does not exists.")


    (* Pretty Printing *)
    fun wrapOpt f dOpt =
      case dOpt of
	NONE => ""
      | SOME d => f d 

    val ppIso = Date.fmt "%Y-%m-%d"
    val ppDk  = Date.fmt "%d/%m-%Y"

    val exampleDk  = "DD/MM-YYYY"
    val exampleIso = "YYYY-MM-DD"

    fun ppMthDk mth =
      case mth of
	Date.Jan => "januar"
      | Date.Feb => "februar"
      | Date.Mar => "marts"
      | Date.Apr => "april"
      | Date.May => "maj"
      | Date.Jun => "juni"
      | Date.Jul => "juli"
      | Date.Aug => "august"
      | Date.Sep => "september"
      | Date.Oct => "oktober"
      | Date.Nov => "november"
      | Date.Dec => "december"

    fun ppMthEng mth =
      case mth of
	Date.Jan => "January"
      | Date.Feb => "February"
      | Date.Mar => "March"
      | Date.Apr => "April"
      | Date.May => "May"
      | Date.Jun => "June"
      | Date.Jul => "July"
      | Date.Aug => "August"
      | Date.Sep => "September"
      | Date.Oct => "October"
      | Date.Nov => "November"
      | Date.Dec => "December"

    fun ppMth mth =
      case ScsLogin.user_lang() of
	ScsLang.da => ppMthDk mth
      | ScsLang.en => ppMthEng mth

    fun ppMthFromDate d = ppMth (Date.month d)

    fun ppWeekdayDk d =
      case d of
	Date.Mon => "Mandag"
      | Date.Tue => "Tirsdag"
      | Date.Wed => "Onsdag"
      | Date.Thu => "Torsdag"
      | Date.Fri => "Fredag"
      | Date.Sat => "Lørdag"
      | Date.Sun => "Søndag"
	
    fun ppDayDk d = ppWeekdayDk (Date.weekDay d)

    fun ppWeekdayEng d =
      case d of
	Date.Mon => "Monday"
      | Date.Tue => "Tuesday"
      | Date.Wed => "Wednesday"
      | Date.Thu => "Thursday"
      | Date.Fri => "Friday"
      | Date.Sat => "Saturday"
      | Date.Sun => "Sunday"

    fun ppWeekday w =
      case ScsLogin.user_lang() of
	ScsLang.da => ppWeekdayDk w
      | ScsLang.en => ppWeekdayEng w

    fun ppWeekdayOneLetter w = Char.toString(String.sub (ppWeekday w,0))

    fun ppDayEng d = ppWeekdayEng (Date.weekDay d)

    fun ppDay d = 
      case ScsLogin.user_lang() of
	ScsLang.da => ppDayDk d
      | ScsLang.en => ppDayEng d

    fun ppHourMin d =
      let
	fun pad2 i = (if i < 10 then "0" else "") ^ (Int.toString i)
      in
	case ScsLogin.user_lang() of
	  ScsLang.da => (pad2 (Date.hour d)) ^ "." ^ (pad2 (Date.minute d))
	| ScsLang.en => (pad2 (Date.hour d)) ^ ":" ^ (pad2 (Date.minute d))
      end

    fun ppLongDk d = (Int.toString (Date.day d)) ^ ". " ^(ppMthDk (Date.month d)) ^ " " ^ (Date.fmt "%Y" d)

    fun ppLongEng d = (Int.toString (Date.day d)) ^ " " ^(ppMthEng (Date.month d)) ^ " " ^ (Date.fmt "%Y" d)

    fun ppLong s = 
      case ScsLogin.user_lang() of
	ScsLang.da => ppLongDk s
      | ScsLang.en => ppLongEng s

    fun ppLongWithWeekdayDk d = ppDayDk d ^ " den " ^ ppLongDk d

    fun ppLongWithWeekdayEng d = ppDayEng d ^ ", " ^ ppLongEng d

    fun ppLongWithWeekday s = 
      case ScsLogin.user_lang() of
	ScsLang.da => ppLongWithWeekdayDk s
      | ScsLang.en => ppLongWithWeekdayEng s

    fun pp s = 
      case ScsLogin.user_lang() of
	ScsLang.da => ppDk s
      | ScsLang.en => ppIso s

    fun ppTimestamp s = Date.fmt "%H:%M.%S" s ^ " " ^ pp s

    fun ppDb s =
      case s
	of SOME d => pp d
      | _ => ""

    fun ppDbExamples s =
      case s
	of SOME d => pp d
      | _ => ( case ScsLogin.user_lang() of
	   	  ScsLang.da => exampleDk
		| ScsLang.en => exampleIso
      )

    fun ppTimestampDb s =
      case s
	of SOME d => ppTimestamp d
      | _ => ""

    fun dateOk (d,m,y) =
      (* Date.sml requires y >= 1900 for some reson??? *)
      m >= 1 andalso m <= 12 andalso d >= 1 andalso d <= daysInMonth y m andalso y >= 1900 

    fun timeOk (h,m,s) =
      h >= 0 andalso h < 24 andalso m>= 0 andalso m < 60 andalso s >= 0 andalso s < 60

    (* Given a date (d,m,y), return the number of days since 01.01.0000 *)
    fun preceedingDays (d,m,y) =
      let
	(* days in last month *)
	val days = d - 1
	(* days in preceeding months *)
	val mm = m - 1
	fun loop (0,acc) = acc
	  | loop (mm,acc) = loop (mm-1,acc + daysInMonth y mm)
	val days = loop (mm,days)
	(* days in preceeding years *)
	val days = 365 * y + preceedingLeaps y
      in
	days
      end

    fun dateCmp (d1,d2) =
      Date.compare (genDate(Date.day d1, mthFromName(Date.month d1),Date.year d1),
		    genDate(Date.day d2, mthFromName(Date.month d2),Date.year d2))

    fun dateInPeriod (d,start_date,end_date) =
      if Date.compare(d,start_date) = General.LESS orelse 
	  Date.compare(end_date,d) = General.LESS then
	  false
	else
	  true

    fun currDateInPeriod (start_date, end_date) =
      dateInPeriod(now_local(),start_date,end_date)

    (* Return all years in a period *)
    fun yearsInPeriod (start_date,end_date) =
      let
	fun loop (y,acc) =
	  if y > Date.year start_date then
	    loop(y-1,y::acc)
	  else
	    y::acc
      in
	loop (Date.year end_date,[])
      end

    fun nearstDayInPeriod(d,start_date,end_date) =
      if dateCmp(d,start_date) = General.LESS then
	start_date
      else
	if dateCmp(d,end_date) = General.GREATER then
	  end_date
	else
	  d

    (* Return first month the the year in the period *)
    fun firstMthInYearInPeriod (year,start_date,end_date) =
      if year = Date.year start_date then
	mthFromName (Date.month start_date)
      else
	mthFromName Date.Jan

    (* Return last month the the year in the period *)
    fun lastMthInYearInPeriod (year,start_date,end_date) =
      if year = Date.year end_date then
	mthFromName (Date.month end_date)
      else
	mthFromName Date.Dec

    (* Return list with all months in a year in the period *)
    fun allMthsInYearInPeriod (year,start_date,end_date) =
      let
	val first_mth = firstMthInYearInPeriod(year,start_date,end_date)
	val last_mth = lastMthInYearInPeriod(year,start_date,end_date)
	fun loop (y,acc) =
	  if y > first_mth then
	    loop(y-1,y::acc)
	  else
	    y::acc
      in
	loop(last_mth,[])
      end

    fun add_secs d n =
      (if n < 0 then
	 Date.fromTimeLocal(Time.- (Date.toTime d, Time.fromSeconds(Int.abs n)))
       else
	 Date.fromTimeLocal(Time.+ (Time.fromSeconds n, Date.toTime d)))
	 handle _ => raise ScsDate ("add_secs. can't add " ^ (Int.toString n) ^
				    " secs to the date " ^ (ppIso d))

    fun add_days d n = add_secs d (n * 24 * 3600)
(*      (if n < 0 then
	 Date.fromTimeLocal(Time.- (Date.toTime d, Time.fromSeconds(Int.abs n * 24 * 3600)))
       else
	 Date.fromTimeLocal (Time.+ (Time.fromSeconds(n * 24 * 3600),Date.toTime d)))
	 handle _ => raise ScsDate ("add_days: can't add " ^ (Int.toString n) ^ 
				    " days to the date " ^ (ppIso d)) 2004-10-04, nh *)

    fun half_year d =
      let
	val year = Date.year d
	val p_first = genDate(1,1,year)
	val p_mid = genDate(30,6,year)
	val p_end = genDate(31,12,year)
      in
	case Date.compare (d,p_mid) of
	  LESS => (p_first,p_mid)
	| EQUAL => (p_first,p_mid)
	| GREATER => (add_days p_mid 1,p_end)
      end

    fun semester d =
      let
	val year = Date.year d
	val p_first = genDate(1,2,year)
	val p_mid = genDate(31,7,year)
	val p_end = genDate(31,1,year+1)
      in
	case Date.compare (d,p_mid) of
	  LESS => (p_first,p_mid)
	| EQUAL => (p_first,p_mid)
	| GREATER => (add_days p_mid 1,p_end)
      end

    (* Monday as first day of week *)
    fun getWeekNo d = 
      Option.valOf (Int.fromString (Date.fmt "%W" d))
      handle _ => raise ScsDate ("ScsDate.getWeekNo. can't calculate week number of " ^ ppIso d)

    fun getDayOfWeek d =
      case Date.weekDay d of
	Date.Mon => 0
      | Date.Tue => 1
      | Date.Wed => 2
      | Date.Thu => 3
      | Date.Fri => 4
      | Date.Sat => 5
      | Date.Sun => 6

    (* Monday as first day of week *)
    fun firstDateInWeek d = add_days d (0-(getDayOfWeek d))

    (* Sunday as last day of week *)
    fun lastDateInWeek d = add_days d (6 - (getDayOfWeek d))

    fun getDayOfMonth d = Date.day d
    fun firstDateInMonth d = genDate(1,mthFromName (Date.month d), Date.year d)
    fun lastDateInMonth d = genDate(daysInMonth (Date.year d) (mthFromName (Date.month d)),
				    mthFromName (Date.month d), Date.year d)
    fun min(d1,d2) =
      if Date.compare (d1,d2) = General.LESS then 
	d1 
      else
	d2
    fun max(d1,d2) =
      if Date.compare (d1,d2) = General.GREATER then
	d1
      else
	d2

    fun sameDayPrevMth d =
      if Date.month d = Date.Jan then
	genDate(Date.day d,12, Date.year d - 1)
      else
	genDate(Int.min(Date.day d,daysInMonth (Date.year d) (mthFromName(Date.month d) - 1)),
		mthFromName(Date.month d) - 1,Date.year d)

    fun sameDayNextMth d =
      if Date.month d = Date.Dec then
	genDate(Date.day d,1, Date.year d + 1)
      else
	genDate(Int.min(Date.day d,daysInMonth (Date.year d) (mthFromName(Date.month d) + 1)),
		mthFromName(Date.month d) + 1,Date.year d)

    fun weekday_from_DB "Mon" = SOME Date.Mon
      | weekday_from_DB "Tue" = SOME Date.Tue
      | weekday_from_DB "Wed" = SOME Date.Wed
      | weekday_from_DB "Thu" = SOME Date.Thu
      | weekday_from_DB "Fri" = SOME Date.Fri
      | weekday_from_DB "Sat" = SOME Date.Sat
      | weekday_from_DB "Sun" = SOME Date.Sun
      | weekday_from_DB s     = NONE

    fun weekday_to_DB Date.Mon = "Mon"
      | weekday_to_DB Date.Tue = "Tue"
      | weekday_to_DB Date.Wed = "Wed"
      | weekday_to_DB Date.Thu = "Thu"
      | weekday_to_DB Date.Fri = "Fri"
      | weekday_to_DB Date.Sat = "Sat"
      | weekday_to_DB Date.Sun = "Sun"

  end (* of structure *)


