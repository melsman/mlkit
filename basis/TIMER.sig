(** Operations for starting and checking timers.

The Timer structure provides facilities for measuring the passing of
wall clock (real) time and the amount of time the running process has
had the CPU (user time), has been active in the OS kernel (system
time), and has spent on garbage collection (GC time).
*)

signature TIMER =
  sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimer : cpu_timer -> {usr : Time.time, sys : Time.time}
    val checkCPUTimes : cpu_timer -> {nongc : {usr : Time.time, sys : Time.time},
                                      gc : {usr : Time.time, sys : Time.time}}
    val checkGCTime : cpu_timer -> Time.time
    val totalCPUTimer : unit -> cpu_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer
  end

(**

[type cpu_timer] The type of CPU timers.

[type real_timer] The type of wall clock (real) timers.

[startCPUTimer()] returns a CPU timer that measures the time the
process is computing (has control of the CPU) starting at this call.

[checkCPUTimer timer] returns the user time (usr) and system time
(sys) that have accumulated since the timer timer was started.

[checkCPUTimes timer] returns the user and system time that have
accumulated since the timer was started, split into the time spent
outside the garbage collector (nongc) and inside it (gc); the sums are
what checkCPUTimer returns.

[checkGCTime timer] returns the user time spent in the garbage
collector since the timer was started.

[totalCPUTimer()] returns a CPU timer that measures the time the
process is computing (has control of the CPU) starting at some
system-dependent initialization time.

[startRealTimer()] returns a wall clock (real) timer that measures how
much time passes, starting from the time of this call.

[checkRealTimer rt] returns the amount of (real) time that has passed
since the timer rt was started.

[totalRealTimer()] returns a wall clock (real) timer that measures how
much time passes, starting from some system-dependent initialization
time.

*)
