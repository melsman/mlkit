(** Symbolic names of POSIX signals.

The structure Posix.Signal defines the symbolic names of all the POSIX
signals (see Section 3.3 of the POSIX standard 1003.1,1996), and
provides conversion functions between them and their underlying
representations.

*)

signature POSIX_SIGNAL =
  sig
    eqtype signal

    val toWord   : signal -> SysWord.word
    val fromWord : SysWord.word -> signal

    val abrt : signal
    val alrm : signal
    val bus  : signal
    val fpe  : signal
    val hup  : signal
    val ill  : signal
    val int  : signal
    val kill : signal
    val pipe : signal
    val quit : signal
    val segv : signal
    val term : signal
    val usr1 : signal
    val usr2 : signal
    val chld : signal
    val cont : signal
    val stop : signal
    val tstp : signal
    val ttin : signal
    val ttou : signal
  end

(**

[eqtype signal] The type of a POSIX signal, an asynchronous
notification of an event.

[toWord signal]
[fromWord w]

These funtions convert between a signal identifier and its underlying
integer representation. Note that fromWord does not check that the
result corresponds to a valid POSIX signal.

[abrt] End process (abort).

[alrm] Alarm clock.

[bus] Bus error.

[fpe] Floating-point exception.

[hup] Hangup.

[ill] Illegal instruction.

[int] Interrupt.

[kill] Kill. (It cannot be caught or ignored.)

[pipe] Write on a pipe when there is no process to read it.

[quit] Quit.

[segv] Segmentation violation.

[term] Software termination signal.

[usr1] User-defined signal 1.

[usr2] User-defined signal 2.

[chld] Sent to parent on child stop or exit.

[cont] Continue if stopped. (It cannot be caught or ignored.)

[stop] Stop. (It cannot be caught or ignored.)

[tstp] Interactive stop.

[ttin] Background read attempted from control terminal.

[ttou] Background write attempted from control terminal.

[Discussion]

The name of the corresponding POSIX signal can be derived by
capitalizing all letters and adding the string ``SIG'' as a
prefix. For example, the POSIX signal associated with usr2 is SIGUSR2.


*)
