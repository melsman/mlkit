signature DATE =
  sig
    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    datatype month = Jan | Feb | Mar | Apr | May | Jun
                   | Jul | Aug | Sep | Oct | Nov | Dec
    type date

    exception Date

    val date : {year : int,
		month : month,
		day : int,
		hour : int,
		minute : int,
		second : int,
		offset : Time.time option} 
	-> date

    val year    : date -> int
    val month   : date -> month
    val day     : date -> int
    val hour    : date -> int
    val minute  : date -> int
    val second  : date -> int
    val weekDay : date -> weekday
    val yearDay : date -> int
    val offset  : date -> Time.time option
    val isDst   : date -> bool option

    val localOffset : unit -> Time.time

    val fromTimeLocal : Time.time -> date
    val fromTimeUniv  : Time.time -> date
    val toTime        : date -> Time.time

    val compare : date * date -> order

    val fmt        : string -> date -> string
    val toString   : date -> string
    val scan       : (char, 'a) StringCvt.reader
                       -> (date, 'a) StringCvt.reader
    val fromString : string -> date option
  end

(*
Description

type date

    An abstract type whose values represents an instant in a specific
    time zone.

val date : {
               year : int,
               month : month,
               day : int,
               hour : int,
               minute : int,
               second : int,
               offset : Time.time option
             } -> date

    creates a canonical date from the given date information. If the
    resulting date is outside the range supported by the
    implementation, the Date exception is raised.

    Seconds outside the range [0,59] are converted to the equivalent
    minutes and added to the minutes argument. Similar conversions are
    performed for minutes to hours, hours to days, days to months, and
    months to years.  Negative values are similarly translated into a
    canonical range, with the extra borrowed from the next larger
    unit. Thus, minute = 10, second = ~140 becomes minute = 7, second
    = 40.

    The offset argument provides time zone information. A value of
    NONE represents the local time zone. A value of SOME(t)
    corresponds to time t west of UTC. In particular,
    SOME(Time.zeroTime) is UTC.  Negative offsets denote time zones to
    the east of UTC, as is traditional. Offsets are taken modulo 24
    hours. That is, we express t, in hours, as sgn(t)(24*d + r), where
    d and r are non-negative, d is integral, and r < 24. The offset
    then becomes sgn(t)*r and sgn(t)(24*d) is added to the hours
    (before converting hours to days).

    Leap years follow the Gregorian calendar. Leap seconds may or may
    not be ignored. In an implementation that takes account of leap
    seconds, the second function may return 60 or 61 in the rare cases
    that this is appropriate.

val year : date -> int
val month : date -> month
val day : date -> int
val hour : date -> int
val minute : date -> int
val second : date -> int
val weekDay : date -> weekday
val yearDay : date -> int
val offset : date -> Time.time option
val isDst : date -> bool option

    These functions extract the attributes of a date value. The year
    returned by year uses year 0 as its base. Thus, the date Robin
    Milnerreceived the Turing award would have year 1991. The function
    yearDay returns the day of the year, starting from 0, i.e., 1
    January is day 0. The value returned by offset reports time zone
    information as the amount of time west of UTC. A value of NONE
    represents the local time zone. The function isDst returns NONE if
    the system has no information concerning daylight savings time.
    Otherwise, it returns SOME(dst) where dst is true if daylight
    savings time is in effect.

val localOffset : unit -> Time.time

    The offset from UTC for the local time zone.

fromTimeLocal t
fromTimeUniv t

    These convert the (UTC) time t into a corresponding
    date. fromTimeLocal represents the date in the local time zone; it
    is the analogue of the ISO C function localtime. The returned date
    will have offset=NONE. fromTimeUniv returns the date in the UTC
    time zone; it is the analogue of the ISO C function gmtime. The
    returned date will have offset=SOME(0).

    If these functions are applied to the same time value, the
    resulting dates will differ by the offset of the local time zone
    from UTC.

toTime date

    returns the (UTC) time corresponding to the date date. It raises
    Date if the date date cannot be represented as a Time.time
    value. It is the analogue of the ISO C function mktime.

compare (date1, date2)

    returns LESS, EQUAL, or GREATER, according as date1 precedes,
    equals, or follows date2 in time. It lexicographically compares
    the dates, using the year, month, day, hour, minute, and second
    information, but ignoring the offset and daylight savings time
    information. It does not detect invalid dates.

    In order to compare dates in two different time zones, the user
    would have to handle the normalization.

fmt s date
toString date

    These return a string representation of the date date. The result
    may be wrong if the date is outside the representable Time.time
    range. They raise Date if the given date is invalid.

    The former formats the date according to the format string s,
    following the semantics of the ISO C function strftime. In
    particular, fmt is locale-dependent. The allowed formats are: %a
    locale's abbreviated weekday name %A locale's full weekday name %b
    locale's abbreviated month name %B locale's full month name %c
    locale's date and time representation (e.g., "Dec 2 06:55:15
    1979") %d day of month [01-31] %H hour [00-23] %I hour [01-12] %j
    day of year [001-366] %m month number [01-12] %M minutes [00-59]
    %p locale's equivalent of the AM/PM designation %S seconds [00-61]
    %U week number of year [00-53], with the first Sunday as the first
    day of week 01 %w day of week [0-6], with 0 representing Sunday %W
    week number of year [00-53], with the first Monday as the first
    day of week 01 %x locale's appropriate date representation %X
    locale's appropriate time representation %y year of century
    [00-99] %Y year including century (e.g., 1997) %Z time zone name
    or abbreviation, or the empty string if no time zone information
    exists %% the percent character %c the character c, if c is not
    one of the format characters listed above For instance, fmt "%A"
    date returns the full name of the weekday specified by date (e.g.,
    "Monday"). For a full description of the format-string syntax,
    consult a description of strftime. Note, however, that unlike
    strftime, the behavior of fmt is defined for the directive %c for
    any character c.

    toString returns a 24-character string representing the date date
    in the following format:

       "Wed Mar 08 19:06:45 1995"

    The function is equivalent to Date.fmt "%a %b %d %H:%M:%S %Y".

scan getc strm
fromString s

    These scan a 24-character date from a character source after
    ignoring possible initial whitespace. The format of the string
    must be precisely as produced by toString. In particular, the
    functions do not parse time zone abbreviations. No check of the
    consistency of the date (weekday, date in the month, ...) is
    performed. If the scanning fails, NONE is returned.

    The function scan takes a character stream reader getc and a
    stream strm.  In case of success, it returns SOME(date, rest),
    where date is the scanned date and rest is the remainder of the
    stream.

    The function fromString takes a string s as its source of
    characters. It is equivalent to StringCvt.scanString scan.
*)
