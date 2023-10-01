(** Imperative IO operations.

The IMPERATIVE_IO signature defines the interface of the Imperative
I/O layer in the I/O stack. This layer provides buffered I/O using
mutable, redirectable streams.

*)

signature IMPERATIVE_IO =
  sig
    structure StreamIO : STREAM_IO

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input        : instream -> vector
    val input1       : instream -> elem option
    val inputN       : instream * int -> vector
    val inputAll     : instream -> vector
    val canInput     : instream * int -> int option
    val lookahead    : instream -> elem option
    val closeIn      : instream -> unit
    val endOfStream  : instream -> bool

    val output       : outstream * vector -> unit
    val output1      : outstream * elem -> unit
    val flushOut     : outstream -> unit
    val closeOut     : outstream -> unit

    val mkInstream   : StreamIO.instream -> instream
    val getInstream  : instream -> StreamIO.instream
    val setInstream  : instream * StreamIO.instream -> unit

    val mkOutstream  : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val getPosOut    : outstream -> StreamIO.out_pos
    val setPosOut    : outstream * StreamIO.out_pos -> unit
  end

(**

[structure StreamIO] This substructure provides lower-level stream
I/O, as defined by the STREAM_IO interface, which is compatible with
the instream and outstream types, in the sense that the conversion
functions mkInstream, getInstream, mkOutstream, and getOutstream allow
the programmer to convert between low-level streams and redirectable
streams. Typically, the redirectable streams are implemented in terms
of low-level streams. Note that StreamIO.outstream is not a functional
stream. The only difference between a StreamIO.outstream and an
outstream is that the latter can be redirected.

[type vector]
[type elem]

These are the abstract types of stream elements and vectors of
elements. For text streams, these are Char.char and String.string,
while for binary streams, they correspond to Word8.word and
Word8Vector.vector.

[type instream] The type of redirectable imperative input streams. Two
imperative streams may share an underlying functional stream or
reader. Closing one of them effectively closes the underlying
functional stream, which will affect subsequent operations on the
other.

[type outstream] The type of redirectable output streams. Two
redirectable streams may share an underlying stream or writer. If this
is the case, writing or positioning the file pointer on one of them,
or closing it, also affects the other.

[input strm] attempts to read from strm, starting from the current
input file position. When elements are available, it returns a vector
of at least one element. When strm is at end-of-stream or is closed,
it returns an empty vector. Otherwise, input blocks until one of these
conditions is met, and returns accordingly. It may raise the exception
Io.

[input1 strm] reads one element from strm. It returns SOME(e) if one
element was available; it returns NONE if at end-of-stream. It may
block, and may raise the exception Io.  After a call to input1
returning NONE to indicate an end-of-stream, the input stream should
be positioned after the end-of-stream.

[inputN (strm, n)] reads at most n elements from strm. It returns a
vector containing n elements if at least n elements are available
before end-of-stream; it returns a shorter (and possibly empty) vector
of all elements remaining before end-of-stream otherwise. It may
block, and may raise the exception Io. It raises Size if n < 0 or if n
is greater than the maxLen value for the vector type.

[inputAll strm] returns all elements of strm up to end-of-stream. It
may block, and may raise the exception Io. It raises Size if the
amount of data exceeds the maxLen of the vector type.

[canInput (strm, n)] returns NONE if any attempt at input would
block. It returns SOME(k), where 0 <= k <= n, if a call to input would
return immediately with at least k characters. Note that k = 0
corresponds to the stream being at end-of-stream.  Some streams may
not support this operation, in which case the Io exception will be
raised. This function also raises the Io exception if there is an
error in the underlying system calls. It raises the Size exception if
n < 0.

    Implementation note:

    It is suggested that implementations of canInput should attempt to
    return as large a k as possible. For example, if the buffer
    contains 10 characters and the user calls canInput (f, 15),
    canInput should call readVecNB(5) to see if an additional 5
    characters are available.



[lookahead strm] determines whether one element is available on strm
before end-of-stream and returns SOME(e) in this case; it returns NONE
if at end-of-stream. In the former case, e is not removed from strm
but stays available for further input operations. It may block, and
may raise the exception Io.  The underlying STREAM_IO stream can be
used to easily implement arbitrary lookahead.

[closeIn strm] closes the input stream strm, freeing resources of the
underlying I/O layers associated with it. Closing an already closed
stream will be ignored. Other operations on a closed stream will
behave as if the stream is at end-of-stream. The function is
implemented in terms of StreamIO.closeIn. It may also raise Io when
another error occurs.

[endOfStream strm] returns true if strm is at end-of-stream, and false
if elements are still available. It may block until one of these
conditions is determined, and may raise the exception Io.  When
endOfStream returns true on an untruncated stream, this denotes the
current situation. After a read from strm to consume the
end-of-stream, it is possible that the next call to endOfStream strm
may return false, and input operations will deliver new elements. For
further information, consult the description of STREAM_IO.endOfStream.

[output (strm, vec)] attempts to write the contents of vec to strm,
starting from the current output file position. It may block until the
underlying layers (and eventually the operating system) can accept all
of vec. It may raise the exception Io. In that case, it is unspecified
how much of vec was actually written.

[output1 (strm, el)] writes exactly one element el to strm. It may
block, and may raise the exception Io if an error occurs. In that
case, it is unspecified how much of el was actually written,
especially if its physical representation is larger than just one
byte. At this level, more than this cannot be guaranteed. Programs
that need more control over this possibility need to make use of more
primitive or OS-specific I/O routines.

[flushOut strm] causes any buffers associated with strm to be written
out. It is implemented in terms of StreamIO.flushOut. The function may
block, and may raise the exception Io when an error occurs.

[closeOut strm] flushes any buffers associated with strm, then closes
strm, freeing resources of the underlying I/O layers associated with
it. It is implemented in terms of StreamIO.closeOut. A write attempt
on a closed outstream will cause the exception
Io{cause=ClosedStream,...} to be raised. It may also raise Io if
another error occurs (e.g., buffers cannot be flushed out).

[mkInstream strm] constructs a redirectable input stream from a
functional one. The current version of strm returned by input
operations will be kept internally and used for the next input. They
can be obtained by getInstream.

[getInstream strm] returns the current version of the underlying
functional input stream of strm. Using getInstream, it is possible to
get input directly from the underlying functional stream. After having
done so, it may be necessary to reassign the newly obtained functional
stream to strm using setInstream; otherwise the previous input will be
read again when reading from strm the next time.

[setInstream (strm, strm')] assigns a new functional stream strm' to
strm. Future input on strm will be read from strm'. This is useful for
redirecting input or interleaving input from different streams, e.g.,
when handling nested include files in a lexer.

[mkOutstream strm] constructs a redirectable output stream from a
low-level functional one. Output to the imperative stream will be
redirected to strm.

[getOutstream strm] flushes strm and returns the underlying StreamIO
output stream. Using getOutstream, it is possible to write output
directly to the underlying stream, or to save it and restore it using
setOutstream after strm has been redirected.

[setOutstream (strm, strm')] flushes the stream underlying strm, and
then assigns a new low-level stream strm' to it. Future output on strm
will be redirected to strm'.

[getPosOut strm] returns the current position in the stream strm. This
raises the exception Io if the stream does not support the operation,
among other reasons. See StreamIO.getPosOut.

[setPosOut (strm, pos)] sets the current position of the stream strm
to be pos. This raises the exception Io if the stream does not support
the operation, among other reasons. See StreamIO.setPosOut.

*)

signature IMPERATIVE_IO_EXTRA =
   sig
      structure StreamIO: STREAM_IO_EXTRA

      type elem = StreamIO.elem
      type instream
      type outstream
      type vector = StreamIO.vector
      type vector_slice = StreamIO.vector_slice

      val canInput: instream * int -> int option
      val closeIn: instream -> unit
      val closeOut: outstream -> unit
      val endOfStream: instream -> bool
      val equalsIn: instream * instream -> bool
      val flushOut: outstream -> unit
      val getInstream: instream -> StreamIO.instream
      val getOutstream: outstream -> StreamIO.outstream
      val getPosOut: outstream -> StreamIO.out_pos
      val inFd: instream -> Posix.IO.file_desc
      val input1: instream -> elem option
      val input: instream -> vector
      val inputAll: instream -> vector
      val inputLine: instream -> vector option
      val inputN: instream * int -> vector
      val lookahead: instream -> elem option
      val mkInstream: StreamIO.instream -> instream
      val mkOutstream: StreamIO.outstream -> outstream
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val openAppend: string -> outstream
      val openIn: string -> instream
      val openOut: string -> outstream
      val openVector: vector -> instream
      val outFd: outstream -> Posix.IO.file_desc
      val output1: outstream * elem -> unit
      val output: outstream * vector -> unit
      val outputSlice: outstream * vector_slice -> unit
      val scanStream:
         ((elem, StreamIO.instream) StringCvt.reader
          -> ('a, StreamIO.instream) StringCvt.reader)
         -> instream -> 'a option
      val setInstream: instream * StreamIO.instream -> unit
      val setOutstream: outstream * StreamIO.outstream -> unit
      val setPosOut: outstream * StreamIO.out_pos -> unit
      val stdErr: outstream
      val stdIn: instream
      val stdOut: outstream
   end
