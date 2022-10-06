(* Date -- 1995-07-03, 1998-04-07 *)

structure Date :> DATE =
  struct

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month = Jan | Feb | Mar | Apr | May | Jun
                   | Jul | Aug | Sep | Oct | Nov | Dec

    datatype date = DATE of {
	year   : int,			(* e.g. 1995 *)
	month  : month,
	day    : int,       		(* 1-31  *)
	hour   : int,       		(* 0-23  *)
	minute : int,       		(* 0-59  *)
	second : int,       		(* 0-61 (allowing for leap seconds) *)
	wday   : weekday,
	yday   : int,		        (* 0-365 *)
	isDst  : bool option,		(* daylight savings time in force *)
	offset : int option			(* signed seconds East of UTC: this
					       zone = UTC+t; ~43200 < t <= 43200 *)
      }

    exception Date

    (* 86400 = 24*60*6 is the number of seconds per day *)

    type tmoz = {tm_hour   : int,
		 tm_isdst  : int,	(* 0 = no, 1 = yes, ~1 = don't know *)
		 tm_mday   : int,
		 tm_min    : int,
		 tm_mon    : int,
		 tm_sec    : int,
		 tm_wday   : int,
		 tm_yday   : int,
		 tm_year   : int
		 }

    fun getCtx () : foreignptr = prim("__get_ctx",())

    fun getlocaltime_ (r : real) : tmoz = prim("sml_localtime", r)
    fun getunivtime_ (r : real) : tmoz = prim("sml_gmtime", r)
    fun mktime_ (t : tmoz) : real = prim("sml_mktime", t)

    (* The offset to add to local time to get UTC: positive West of UTC *)
    val localoffset = Initial.localoffset

    local val asctime_exn = Initial.fail_asctime
          val strftime_exn = Initial.fail_strftime
    in fun asctime_ (t : tmoz) : string = prim("sml_asctime", (getCtx(),t,asctime_exn))
       fun strftime_ (s : string, t : tmoz) : string = prim("sml_strftime", (getCtx(),s,t,Overflow(*strftime_exn*)))
    end

    exception Date_toweekday
    exception Date_tomonth

    val toweekday = fn 0 => Sun | 1 => Mon | 2 => Tue | 3 => Wed
                     | 4 => Thu | 5 => Fri | 6 => Sat
		     | _ => raise Date_toweekday
    val fromwday  = fn Sun => 0 | Mon => 1 | Tue => 2 | Wed => 3
                     | Thu => 4 | Fri => 5 | Sat => 6;
    val tomonth   = fn 0 => Jan | 1 => Feb |  2 => Mar |  3 => Apr
                     | 4 => May | 5 => Jun |  6 => Jul |  7 => Aug
		     | 8 => Sep | 9 => Oct | 10 => Nov | 11 => Dec
		     | _ => raise Date_tomonth
    val frommonth = fn Jan => 0 | Feb => 1 | Mar => 2  | Apr => 3
		     | May => 4 | Jun => 5 | Jul => 6  | Aug => 7
		     | Sep => 8 | Oct => 9 | Nov => 10 | Dec => 11;

    fun tmozToDate {tm_hour, tm_isdst, tm_mday, tm_min, tm_mon, tm_sec,
		    tm_wday, tm_yday, tm_year} offset =
	DATE {year = tm_year + 1900, month = tomonth tm_mon,
	      day = tm_mday, hour = tm_hour, minute = tm_min,
	      second = tm_sec, wday = toweekday tm_wday,
	      yday = tm_yday,
	      isDst = (case tm_isdst of 0 => SOME false
	                              | 1 => SOME true
                                      | _ => NONE),
	      offset = offset }

    fun leapyear y = y mod 4 = 0 andalso y mod 100 <> 0 orelse y mod 400 = 0

    fun monthdays year month =
      case month
	of Jan => 31 | Mar => 31 | May => 31 | Jul => 31 | Aug => 31 | Oct => 31 | Dec => 31
         | Feb => if leapyear year then 29 else 28
	 | Apr => 30  | Jun => 30 | Sep => 30 | Nov => 30

    fun yeardays year = if leapyear year then 366 else 365

    (* Check whether date may be passed to ISO/ANSI C functions: *)

    fun okDate (DATE {year, month, day, hour, minute, second, ...}) =
	1900 <= year
	andalso 1 <= day    andalso day    <= monthdays year month
	andalso 0 <= hour   andalso hour   <= 23
	andalso 0 <= minute andalso minute <= 59
	andalso 0 <= second andalso second <= 61 (* leap seconds *)

    fun dateToTmoz (dt as DATE {year, month, day, hour, minute, second,
				wday, yday, isDst, offset}) =
	if okDate dt then
	    {tm_hour = hour, tm_mday = day, tm_min = minute,
	     tm_mon = frommonth month, tm_sec = second,
	     tm_year = year - 1900,
	     tm_isdst = case isDst of SOME false=>0 | SOME true=>1 | NONE=> ~1,
 	     tm_wday = fromwday wday, tm_yday = yday}
	else
	    raise Date;

    (* -------------------------------------------------- *)
    (* Translated from Emacs's calendar.el:               *)

    (* Reingold: Number of the day within the year: *)

    fun dayinyear year month day =
	let val monthno = frommonth month
	in
	    day - 1 + 31 * monthno
	    - (if monthno > 1 then
		   (27 + 4 * monthno) div 10 - (if leapyear year then 1 else 0)
	       else 0)
	end

    (* Reingold: Find the number of days elapsed from the (imagined)
       Gregorian date Sunday, December 31, 1 BC to the given date. *)

    fun todaynumber year month day =
        let val prioryears = year - 1
	in
	    dayinyear year month day
	  + 1
	  + 365 * prioryears
	  + prioryears div 4
          - prioryears div 100
          + prioryears div 400
	end

    (* Reingold et al: from absolute day number to year, month, date: *)

    fun fromdaynumber n =
	let val d0 = n - 1
	    val n400 = d0 div 146097
	    val d1 = d0 mod 146097
	    val n100 = d1 div 36524
	    val d2 = d1 mod 36524
	    val n4 = d2 div 1461
	    val d3 = d2 mod 1461
	    val n1 = d3 div 365
	    val day = 1 + d3 mod 365
	    val year = 400 * n400 + 100 * n100 + n4 * 4 + n1 + 1
	    fun loop month day =
		let val mdays = monthdays year (tomonth month)
		in
		    if mdays < day then loop (month+1) (day-mdays)
		    else (year, tomonth month, day)
		end
	in
	    if n100 = 4 orelse n1 = 4 then
		(year-1, Dec, 31)
	    else
		loop 0 day
	end

    (* -------------------------------------------------- *)

    fun weekday daynumber = toweekday (daynumber mod 7)

    (* Normalize a date, disregarding leap seconds: *)

    fun normalizedate yr0 mo0 dy0 hr0 mn0 sec0 offset =
	let val mn1    = mn0 + sec0 div 60
	    val second = sec0 mod 60
	    val hr1    = hr0 + mn1 div 60
	    val minute = mn1 mod 60
	    val dayno  = todaynumber yr0 mo0 dy0 + hr1 div 24
	    val hour   = hr1 mod 24
	    val (year, month, day) = fromdaynumber dayno
	    val date1 = DATE {
			      year   = year,
			      month  = month,
			      day    = day,
			      hour   = hour,
			      minute = minute,
			      second = second,
			      wday   = weekday dayno,
			      yday   = dayinyear year month day,
			      offset = offset,
			      isDst  = case offset of
			                   NONE   => NONE
					 | SOME _ => SOME false }
	in
            (* One cannot reliably compute DST in non-local timezones,
	    not even given the offset from UTC.  Countries in the
	    Northern hemisphere have DST during Mar-Oct, those around
	    Equator do not have DST, and those in the Southern
	    hemisphere have DST during Oct-Mar. *)

	    if year < 1970 orelse year > 2037 then date1
	    else
		case offset of
		    NONE   =>
			tmozToDate (getlocaltime_ (mktime_ (dateToTmoz date1)))
			           offset
		  | SOME t => date1
	end

    fun fromTimeLocal t =
	tmozToDate (getlocaltime_ (Time.toReal t)) NONE;

    fun fromTimeUniv t =
	tmozToDate (getunivtime_ (Time.toReal t)) (SOME 0);

    (* The following implements conversion from a local date to
       a Time.time.  It IGNORES wday and yday.  *)

    fun toTime (date as DATE {offset, ...}) =
	let val secoffset =
	    case offset of
		NONE      => 0.0
	      | SOME secs => localoffset + real secs
	    val clock = mktime_ (dateToTmoz date) - secoffset
	in
	    if clock < 0.0 then raise Date
	    else Time.fromReal clock
	end;

    fun localOffset () = Time.fromSeconds (LargeInt.fromInt(Real.round localoffset mod 86400))

    exception Date_toString
    fun toString date =
	String.substring(asctime_ (dateToTmoz date), 0, 24)
	handle Fail _    => raise Date
	     | Subscript => raise Date_toString

    fun fmt fmtstr date =
	(strftime_ (fmtstr, dateToTmoz date))
	handle Fail _ => raise Date

    (* To scan dates in the format "Wed Mar  8 19:06:45 1995" *)

    exception BadFormat;
    fun getVal (SOME v) = v
      | getVal NONE     = raise BadFormat;

    fun scan getc src =
    let val getstring  = StringCvt.splitl Char.isAlpha getc
	fun getint src = getVal (Int.scan StringCvt.DEC getc src)
	fun drop p     = StringCvt.dropl p getc
	fun isColon c  = (c = #":")

	val getMonth = fn "Jan" => Jan | "Feb" => Feb | "Mar" => Mar
                     | "Apr" => Apr | "May" => May | "Jun" => Jun
		     | "Jul" => Jul | "Aug" => Aug | "Sep" => Sep
		     | "Oct" => Oct | "Nov" => Nov | "Dec" => Dec
		     | _ => raise BadFormat
	val getWday  = fn "Sun" => Sun | "Mon" => Mon | "Tue" => Tue
		     | "Wed" => Wed | "Thu" => Thu | "Fri" => Fri
		     | "Sat" => Sat
		     | _ => raise BadFormat

	val (wday, src1)  = getstring src
	val (month, src2) = getstring (drop Char.isSpace src1)
	val (day, src3)   = getint src2
	val (hour, src4)  = getint src3
	val (min, src5)   = getint (drop isColon src4)
	val (sec, src6)   = getint (drop isColon src5)
	val (year, src7)  = getint src6
	val month         = getMonth month
    in SOME (DATE {year = year, month = month,
		   day = day,  hour = hour, minute = min,
		   second = sec, wday = getWday wday,
		   yday = dayinyear year month day,
		   isDst = NONE, offset = NONE}, src7)
    end
    handle BadFormat => NONE

    fun fromString s = StringCvt.scanString scan s

    (* Ignore timezone and DST when comparing dates: *)

    fun compare
	(DATE {year=y1,month=mo1,day=d1,hour=h1,minute=mi1,second=s1, ...},
	 DATE {year=y2,month=mo2,day=d2,hour=h2,minute=mi2,second=s2, ...}) =
	let fun cmp(v1, v2, cmpnext) =
	    if v1 < v2 then LESS
	    else if v1 > v2 then GREATER
	    else (* EQUAL *) cmpnext ()
	in
	    cmp(y1, y2,
	    fn _ => cmp(frommonth mo1, frommonth mo2,
	    fn _ => cmp(d1, d2,
	    fn _ => cmp(h1, h2,
	    fn _ => cmp(mi1, mi2,
	    fn _ => cmp(s1, s2,
	    fn _ => EQUAL))))))
	end

    fun date { year, month, day, hour, minute, second, offset } =
	if year < 0 then raise Date
	else
	    let val (dayoffset, offset') =
	        case offset of
		    NONE      => (0, NONE)
		  | SOME time =>
			let val secs      = LargeInt.toInt(Time.toSeconds time)
			    val secoffset =
				if secs <= 43200 then ~secs else 86400 - secs
			in (Int.quot(secs, 86400), SOME secoffset) end
		val day' = day + dayoffset
	    in
		normalizedate year month day' hour minute second offset'
	    end

    fun year (DATE { year, ... }) = year

    fun month (DATE { month, ... }) = month

    fun day (DATE { day, ... }) = day

    fun hour (DATE { hour, ... }) = hour

    fun minute (DATE { minute, ... }) = minute

    fun second (DATE { second, ... }) = second

    fun weekDay (DATE { wday, ... }) = wday

    fun yearDay (DATE { yday, ... }) = yday

    fun isDst (DATE { isDst, ... }) = isDst

    fun offset (DATE { offset, ... }) =
	Option.map (fn secs => Time.fromSeconds (LargeInt.fromInt((86400 + secs) mod 86400)))
	           offset
  end
