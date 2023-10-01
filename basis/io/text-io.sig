(** Operations for input and output of characters and strings.

The TEXT_IO interface provides input/output of characters and
strings. Most of the operations themselves are defined in the
IMPERATIVE_IO signature.

The TEXT_IO interface is matched by two structures, the required
TextIO and the optional WideTextIO. The former implements strings
based on the extended ASCII 8-bit characters. The latter provides
strings of characters of some size greater than or equal to 8 bits.

The signature given below for TEXT_IO is not valid SML, in that the
substructure StreamIO is respecified. (It is initially specified as a
substructure having signature STREAM_IO in the included signature
IMPERATIVE_IO.) This abuse of notation seems acceptable in that the
intended meaning is clear (a structure matching TEXT_IO also matches
IMPERATIVE_IO and has a substructure StreamIO that matches
TEXT_STREAM_IO) while avoiding a textual inclusion of the whole
signature of IMPERATIVE_IO except its StreamIO substructure.
*)

signature TEXT_IO =
  sig
      structure StreamIO : TEXT_STREAM_IO
         where type pos = TextPrimIO.pos
         where type reader = TextPrimIO.reader
         where type writer = TextPrimIO.writer

      type elem = StreamIO.elem
      type instream
      type outstream
      type vector = StreamIO.vector

      val canInput     : instream * int -> int option
      val closeIn      : instream -> unit
      val closeOut     : outstream -> unit
      val endOfStream  : instream -> bool
      val flushOut     : outstream -> unit
      val getInstream  : instream -> StreamIO.instream
      val getOutstream : outstream -> StreamIO.outstream
      val getPosOut    : outstream -> StreamIO.out_pos
      val input1       : instream -> elem option
      val input        : instream -> vector
      val inputAll     : instream -> vector
      val inputLine    : instream -> string option
      val inputN       : instream * int -> vector
      val lookahead    : instream -> elem option
      val mkInstream   : StreamIO.instream -> instream
      val mkOutstream  : StreamIO.outstream -> outstream
      val openAppend   : string -> outstream
      val openIn       : string -> instream
      val openOut      : string -> outstream
      val openString   : string -> instream
      val output1      : outstream * elem -> unit
      val output       : outstream * vector -> unit
      val outputSubstr : outstream * substring -> unit
      val scanStream   : ((Char.char, StreamIO.instream) StringCvt.reader
                         -> ('a, StreamIO.instream) StringCvt.reader)
                         -> instream -> 'a option
      val setInstream  : (instream * StreamIO.instream) -> unit
      val setOutstream : outstream * StreamIO.outstream -> unit
      val setPosOut    : outstream * StreamIO.out_pos -> unit
      val stdIn        : instream
      val stdOut       : outstream
      val stdErr       : outstream
      val print        : string -> unit
  end

(**

[openIn s] creates a new instream associated with the file named s.
Raises Io.Io is file s does not exist or is not accessible.

[closeIn istr] closes stream istr.  Has no effect if istr is closed
already.  Further operations on istr will behave as if istr is at
end of stream (that is, will return "" or NONE or true).

[input istr] reads some elements from istr, returning a vector v of
those elements.  The vector will be empty (size v = 0) if and only
if istr is at end of stream or is closed.  May block (not return
until data are available in the external world).

[inputAll istr] reads and returns the string v of all characters
remaining in istr up to end of stream.

[inputNoBlock istr] returns SOME(v) if some elements v can be read
without blocking; returns SOME("") if it can be determined without
blocking that istr is at end of stream; returns NONE otherwise.  If
istr does not support non-blocking input, raises
Io.NonblockingNotSupported.

[input1 istr] returns SOME(e) if at least one element e of istr is
available; returns NONE if istr is at end of stream or is closed;
blocks if necessary until one of these conditions holds.

[inputN(istr, n)] returns the next n characters from istr as a string,
if that many are available; returns all remaining characters if end of
stream is reached before n characters are available; blocks if
necessary until one of these conditions holds.  (This is the behaviour
of the `input' function prescribed in the 1990 Definition of Standard
ML).

[inputLine istr] returns one line of text, including the terminating
newline character.  If end of stream is reached before a newline
character, then the remaining part of the stream is returned, with a
newline character added.  If istr is at end of stream or is closed,
then the empty string "" is returned.

[endOfStream istr] returns false if any elements are available in
istr; returns true if istr is at end of stream or closed; blocks if
necessary until one of these conditions holds.

[lookahead istr] returns SOME(e) where e is the next element in the
stream; returns NONE if istr is at end of stream or is closed; blocks
if necessary until one of these conditions holds.  Does not advance
the stream.

[stdIn] is the buffered state-based standard input stream.

[scanStream scan istr] turns the instream istr into a character source
and applies the scanner `scan' to that source.  See StringCvt for more
on character sources and scanners.  The Moscow ML implementation
currently can backtrack only 512 characters, and raises Fail if the
scanner backtracks further than that.

[openOut s] creates a new outstream associated with the file named s.
If file s does not exist, and the directory exists and is writable,
then a new file is created.  If file s exists, it is truncated (any
existing contents are lost).

[openAppend s] creates a new outstream associated with the file named
s.  If file s does not exist, and the directory exists and is
writable, then a new file is created.  If file s exists, any existing
contents are retained, and output goes at the end of the file.

[closeOut ostr] closes stream ostr; further operations on ostr (except
for additional close operations) will raise exception Io.Io.

[output(ostr, v)] writes the string v on outstream ostr.

[output1(ostr, e)] writes the character e on outstream ostr.

[flushOut ostr] flushes the outstream ostr, so that all data written
to ostr becomes available to the underlying file or device.

[stdOut] is the buffered state-based standard output stream.

[stdErr] is the unbuffered state-based standard error stream.  That
is, it is always kept flushed, so flushOut(stdErr) is redundant.

[print s] prints the string s to stdOut and flushes the stdOut stream.

[Discussion] This structure provides input/output functions on text
streams.  The functions are state-based: reading from or writing to a
stream changes the state of the stream.  The streams are buffered:
output to a stream may not immediately affect the underlying file or
device. Note that under DOS, Windows, OS/2, and MacOS, text streams
will be `translated' by converting (e.g.) the double newline CRLF to a
single newline character \n. Type instream is the type of state-based
characters input streams, and type outstream is the type of
state-based character output streams.  Type elem is the type char of
characters, and type vector is the type of character vectors
(strings).

*)

signature TEXT_IO_EXTRA =
   sig
      include TEXT_IO

      val equalsIn: instream * instream -> bool
      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val outFd: outstream -> Posix.IO.file_desc
   end
