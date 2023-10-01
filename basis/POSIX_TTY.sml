(** Model of the POSIX terminal interface.

The structure Posix.TTY specifies a model of a general terminal
interface, as described in Section 7 of the POSIX standard
1003.1,1996.

*)

signature POSIX_TTY =
  sig
    eqtype pid
    eqtype file_desc

    structure V :
      sig
        val eof   : int
        val eol   : int
        val erase : int
        val intr  : int
        val kill  : int
        val min   : int
        val quit  : int
        val susp  : int
        val time  : int
        val start : int
        val stop  : int
        val nccs  : int
        type cc
        val cc     : (int * char) list -> cc
        val update : cc * (int * char) list -> cc
        val sub    : cc * int -> char
      end

    structure I :
      sig
        include BIT_FLAGS
        val brkint : flags
        val icrnl  : flags
        val ignbrk : flags
        val igncr  : flags
        val ignpar : flags
        val inlcr  : flags
        val inpck  : flags
        val istrip : flags
        val ixoff  : flags
        val ixon   : flags
        val parmrk : flags
      end

    structure O :
      sig
        include BIT_FLAGS
        val opost : flags
      end

    structure C :
      sig
        include BIT_FLAGS
        val clocal : flags
        val cread  : flags
        val cs5    : flags
        val cs6    : flags
        val cs7    : flags
        val cs8    : flags
        val csize  : flags
        val cstopb : flags
        val hupcl  : flags
        val parenb : flags
        val parodd : flags
      end

    structure L :
      sig
        include BIT_FLAGS
        val echo   : flags
        val echoe  : flags
        val echok  : flags
        val echonl : flags
        val icanon : flags
        val iexten : flags
        val isig   : flags
        val noflsh : flags
        val tostop : flags
      end

    eqtype speed

    val compareSpeed : speed * speed -> order
    val speedToWord  : speed -> SysWord.word
    val wordToSpeed  : SysWord.word -> speed

    val b0     : speed
    val b50    : speed
    val b75    : speed
    val b110   : speed
    val b134   : speed
    val b150   : speed
    val b200   : speed
    val b300   : speed
    val b600   : speed
    val b1200  : speed
    val b1800  : speed
    val b2400  : speed
    val b4800  : speed
    val b9600  : speed
    val b19200 : speed
    val b38400 : speed

    type termios

    val termios : { iflag : I.flags,
                    oflag : O.flags,
                    cflag : C.flags,
                    lflag : L.flags,
                    cc : V.cc,
                    ispeed : speed,
                    ospeed : speed
                  } -> termios
    val fieldsOf : termios
                   -> { iflag : I.flags,
                        oflag : O.flags,
                        cflag : C.flags,
                        lflag : L.flags,
                        cc : V.cc,
                        ispeed : speed,
                        ospeed : speed
                      }
    val getiflag : termios -> I.flags
    val getoflag : termios -> O.flags
    val getcflag : termios -> C.flags
    val getlflag : termios -> L.flags
    val getcc    : termios -> V.cc

    structure CF :
      sig
        val getospeed : termios -> speed
        val getispeed : termios -> speed
        val setospeed : termios * speed -> termios
        val setispeed : termios * speed -> termios
      end

    structure TC :
      sig
        eqtype set_action

        val sanow   : set_action
        val sadrain : set_action
        val saflush : set_action

        eqtype flow_action

        val ooff : flow_action
        val oon  : flow_action
        val ioff : flow_action
        val ion  : flow_action

        eqtype queue_sel

        val iflush  : queue_sel
        val oflush  : queue_sel
        val ioflush : queue_sel

        val getattr   : file_desc -> termios
        val setattr   : file_desc * set_action * termios -> unit
        val sendbreak : file_desc * int -> unit
        val drain     : file_desc -> unit
        val flush     : file_desc * queue_sel -> unit
        val flow      : file_desc * flow_action -> unit
        val getpgrp   : file_desc -> pid
        val setpgrp   : file_desc * pid -> unit
      end
  end

(**

[eqtype pid] The type of a process identifier.

[eqtype file_desc] The type of an open file descriptor.

[structure V] The V substructure provides means for specifying the
special control characters.

    [eof]
    [eol]
    [erase]
    [intr]
    [kill]
    [min]
    [quit]
    [susp]
    [time]
    [start]
    [stop]

    Indices for the special control characters EOF, EOL, ERASE, INTR,
    KILL, MIN, QUIT, SUSP, TIME, START, and STOP, respectively. These
    are the indices used in the functions cc and sub.

    [nccs] The total number of special characters. Thus, valid indices
    range from 0 to nccs-1.

    [type cc] A vector of special control characters used by the
    device driver.

    [cc l] creates a value of type cc, mapping an index to its paired
    character. Unspecified indices are associated with #"\000". For
    example, to have the character #"\^D" (control-D) serve as the EOF
    (end-of-file) character, one would use cc [(V.eof, #"\^D")] to
    create a cc value, embed this in a termios type, and invoke
    TC.setattr.

    [update (cs, l)] returns a copy of cs, but with the new mappings
    specified by l overwriting the original mappings.

    [sub (cs, i)] returns the special control character associated in
    cs with the index i. It raises Subscript if i is negative or i >=
    nccs.

[structure I] The I substructure contains flags for specifying input
control. The following table provides a brief description of the
flags.

    Flag name     Description
    brkint        Signal interrupt on break.
    icrnl         Map CR (#"\^M") to NL (#"\n") on input.
    ignbrk        Ignore a break condition.
    igncr         Ignore CR characters.
    ignpar        Ignore characters with parity errors.
    inlcr         Map NL to CR on input.
    inpck         Enable input parity check.
    istrip        Strip the eighth bit of a byte.
    ixoff         Enable start/stop input control.
    ixon          Enable start/stop output control.
    parmrk        Mark parity errors.

[structure O] The O substructure contains flags for specifying output
control.

    [opost] Perform output processing.

[structure C] The C substructure contains flags for specifying basic
terminal hardware control. The following table provides a brief
description of the flags.

    Flag name      Description
    clocal         Ignore modem status lines.
    cread          Enable the receiver.
    csize          Mask for the number of bits per byte used for both
                   transmission and reception. This is the union of
                   cs5, cs6, cs7, and cs8.
    cs5            5 bits per byte.
    cs6            6 bits per byte.
    cs7            7 bits per byte.
    cs8            8 bits per byte.
    cstopb         Specifies sending two stop bits rather than one.
    hupcl          Hang up the modem connection when the last process
                   with the port open closes it.
    parenb         Enable parity generation and detection.
    parodd         Use odd parity rather than even if parenb is set.

[structure L] The L substructure contains flags for specifying various
local control modes. The following table provides a brief description
of the flags.

    Flag name      Description
    echo           Echo input characters back to the terminal.
    echoe          Echo the ERASE character on backspace in canonical
                   mode.
    echok          Echo the KILL character in canonical mode.
    echonl         In canonical mode, echo a NL character even if echo
                   is not set.
    icanon         Set canonical mode, enabling erase and kill
                   processing, and providing line-based input.
    iexten         Enable extended functions.
    isig           Enable input characters to be mapped to signals.
    noflsh         Disable the normal input and output flushing
                   connected with the INTR, QUIT, and SUSP
                   characters. (See the Posix.TTY.V substructure.)
    tostop         Send Posix.Signal.ttou for background output.

[eqtype speed] Terminal input and output baud rates.

[compareSpeed (sp, sp')] returns LESS, EQUAL, or GREATER when the baud
rate sp is less than, equal to, or greater than that of sp',
respectively.

[speedToWord speed]
[wordToSpeed w]

These functions converts between a speed value and its underlying word
representation. No checking is performed by wordToSpeed to ensure the
resulting value corresponds to an allowed speed in the given system.

[type termios] The attributes associated with a terminal. It acts as
an abstract representation of the record used as the argument to the
termios function.

[termios args] This function creates a termios value using the given
flags, special characters, and speeds.

[fieldsOf termios] This function returns a concrete representation of
a termios value.

[getiflag termios]
[getoflag termios]
[getcflag termios]
[getlflag termios]
[getcc termios]

These functions are the obvious projection functions from a termios
value to its constituent fields.

[structure CF] The CF substructure contains functions for getting and
setting the input and output baud rates in a termios value.

    [getospeed termios]
    [getispeed termios]

    These functions return the output and input baud rates,
    respectively, of the argument.

    [setospeed (t, speed)]
    [setispeed (t, speed)]

    These functions return a copy of t, but with the output (input)
    speed set to speed.

[structure TC] The TC substructure contains various types and
functions used for handling terminal line control.

    [eqtype set_action] Values of this type specify the behavior of
    the setattr function.

    [sanow] Changes occur immediately.

    [sadrain] Changes occur after all output is transmitted.

    [saflush] Changes occur after all output is transmitted and after
    all received but unread input is discarded.

    [eqtype flow_action] Values of this type specify the behavior of
    the flow function.

    [ooff] Causes suspension of output.

    [oon] Restarts suspended output.

    [ioff] Causes the transmission of a STOP character to the terminal
    device, to stop it from transmitting data.

    [ion] Causes the transmission of a START character to the terminal
    device, to restart it transmitting data.

    [eqtype queue_sel] Values of this type specify the behavior of the
    flush function.

    [iflush] Causes all data received but not read to be flushed.

    [oflush] Causes all data written but not transmitted to be
    flushed.

    [ioflush] Discards all data written but not transmitted, or
    received but not read.

    [getattr fd] gets the attributes of the terminal associated with
    file descriptor fd.

    [setattr (fd, action, termios)] sets the attributes of the
    terminal associated with file descriptor fd as specified in
    termios. When the change occurs is specified by action.

    [sendbreak (fd, t)] causes the transmission of a sequence of
    zero-valued bits to be sent, if the associated terminal is using
    asynchronous serial data transmission. If t is 0, this will send
    zero-valued bits for at least a quarter second, and no more than
    half a second. If t is not zero, zero-valued bits are transmitted
    for an implementation-defined period of time.

    [drain fd] waits for all output written on fd to be transmitted.

    [flush (fd, qs)] discards any data written but not transmitted, or
    received but not read, depending on the value of qs.

    [flow (fd, action)] suspends and restarts transmission or
    reception of data, depending on the value of action.

    [getpgrp fd] returns the process group ID of the foreground
    process group associated with the terminal attached to fd.

    [setpgrp (fd, pid)] sets the foreground process group ID
    associated with fd to pid.

*)
