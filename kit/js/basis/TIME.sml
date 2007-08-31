signature TIME =
  sig
    eqtype time
    exception Time
    val zeroTime : time
    val fromReal : LargeReal.real -> time
    val toReal : time -> LargeReal.real
    val toSeconds      : time -> LargeInt.int
    val toMilliseconds : time -> LargeInt.int
    val toMicroseconds : time -> LargeInt.int
(*    val toNanoseconds  : time -> LargeInt.int *)
    val fromSeconds      : LargeInt.int -> time
    val fromMilliseconds : LargeInt.int -> time
    val fromMicroseconds : LargeInt.int -> time
(*    val fromNanoseconds  : LargeInt.int -> time *)

    val + : time * time -> time
    val - : time * time -> time

    val compare : time * time -> order
    val <  : time * time -> bool
    val <= : time * time -> bool
    val >  : time * time -> bool
    val >= : time * time -> bool

    val now : unit -> time

    val fmt      : int -> time -> string
    val toString : time -> string
    val scan       : (char, 'a) StringCvt.reader
                       -> (time, 'a) StringCvt.reader
    val fromString : string -> time option 
  end

(*
eqtype time
    The type used to represent both absolute times and durations of time
    intervals, including negative values moving to the past. Absolute times are
    represented in the same way as time intervals, and can be thought of as
    time intervals starting at some fixed reference point.  Their
    discrimination is only conceptual. Consequently, operations can be applied
    to all meaningful combinations (but also meaningless ones) of absolute
    times and intervals.

exception Time
    The exception raised when the result of conversions to time or of
    operations over time is not representable, or when an illegal operation has
    been attempted.

val zeroTime : time
    This denotes both the empty time interval and a common reference point for
    specifying absolute time values. It is equivalent to fromReal(0.0).

    Absolute points on the time scale can be thought of as being represented as
    intervals starting at zeroTime. The function Date.fromTimeLocal can be used
    to see what time zeroTime actually represents in the local timezone.

fromReal r
    converts the real number r to the time value denoting r seconds. Depending
    on the resolution of time, fractions of a microsecond may be lost. It
    raises Time when the result is not representable.

toReal t
    converts the time value t to a real number denoting the value of t in
    seconds. When the type real has less precision than Time.time (for example,
    when it is implemented as a single-precision float), information about
    microseconds or, for very large values, even seconds, may be lost.

toSeconds t
toMilliseconds t
toMicroseconds t
toNanoseconds t
    These functions return the number of full seconds (respectively,
    milliseconds, microseconds, or nanoseconds) in t; fractions of the time
    unit are dropped, i.e., the values are rounded towards 0. Thus, if t
    denotes 2.01 seconds, the functions return 2, 2010, 2010000, and 2010000000
    respectively. When the result is not representable by LargeInt.int, the
    exception Overflow is raised.

fromSeconds n
fromMilliseconds n
fromMicroseconds n
fromNanoseconds n
    These convert the number n to a time value denoting n seconds
    (respectively, milliseconds, microseconds, or nanoseconds). If the result
    is not representable by the time type, then the exception Time is raised.

t1 + t2
    returns a time interval denoting the duration of t1 plus that of t2, when
    both t1 and t2 are interpreted as intervals. Equivalently, when t1 is
    interpreted as an absolute time and t2 as an interval, the absolute time
    that is t2 later than t1 is returned. (Both views are equivalent as
    absolute times are represented as intervals from zeroTime). When the result
    is not representable as a time value, the exception Time is raised. This
    operation is commutative.

t1 - t2
    returns a time interval denoting the duration of t1 minus that of t2, when
    both t1 and t2 are interpreted as intervals. Equivalently, when t1 is
    interpreted as an absolute time and t2 as an interval, the absolute time
    that is t2 earlier than t1 is returned; when both t1 and t2 are interpreted
    as absolute times, the interval between t1 and t2 is returned. (All views
    are equivalent as absolute times are represented as intervals from
    zeroTime). When the result is not representable as a time value, the
    exception Time is raised.

compare (t1, t2)
    returns LESS, EQUAL, or GREATER when the time interval t1 is shorter than,
    of same length as, or longer than t2, respectively, or the absolute time t1
    is earlier than, coincides with, or is later than the absolute time t2.

val < : time * time -> bool
val <= : time * time -> bool
val > : time * time -> bool
val >= : time * time -> bool
    These return true if the corresponding relation holds between the two
    times.

val now : unit -> time
    The current time. This is usually interpreted as an absolute time, the time
    at which the function call was made. Although now does not normally raise
    an exception, this may happen when it is called at a time that is not
    representable.

fmt n t
toString t
    These return a string containing a decimal number representing t in
    seconds. Using fmt, the fractional part is rounded to n decimal digits. If
    n = 0, there should be no fractional part. Having n < 0 causes the Size
    exception to be raised. toString rounds t to 3 decimal digits. It is
    equivalent to fmt 3 t.

        Example:

    fmt 3 (fromReal 1.8) = "1.800"
    fmt 0 (fromReal 1.8) = "2"
    fmt 0 zeroTime = "0"



scan getc src
fromString s
    These functions scan a time value from a character stream or a string. They
    recognize a number of seconds specified as a string that matches the
    regular expression:

        [+~-]?([0-9]+.[0-9]+? | .[0-9]+) 

    Initial whitespace is ignored. Both functions raise Time when the value is
    syntactically correct but not representable.

    The function scan takes a character source src and an reader getc and tries
    to parse a time value from src. It returns SOME(t,r) where t is the time
    value denoted by a prefix of src and r is the rest of src; or it returns
    NONE when no prefix of src is a representation of a time value.

    The function fromString parses a time value from the string s, returning
    SOME(t) where t is the time value denoted by a prefix of s or NONE when no
    prefix of s is a representation of a time value. Note that this function is
    equivalent to StringCvt.scanString scan.

*)
