(** Streaming IO operations.

The STREAM_IO signature defines the interface of the Stream I/O layer
in the I/O stack. This layer provides buffering over the readers and
writers of the Primitive I/O layer.

Input streams are treated in the lazy functional style: that is, input
from a stream f yields a finite vector of elements, plus a new stream
f'. Input from f again will yield the same elements; to advance within
the stream in the usual way, it is necessary to do further input from
f'. This interface allows arbitrary lookahead to be done very cleanly,
which should be useful both for ad hoc lexical analysis and for
table-driven, regular-expression-based lexing.

Output streams are handled more conventionally, since the lazy
functional style does not seem to make sense for output.

Stream I/O functions may raise the Size exception if a resulting
vector of elements would exceed the maximum vector size, or the IO.Io
exception. In general, when IO.Io is raised as a result of a failure
in a lower-level module, the underlying exception is caught and
propagated up as the cause component of the IO.Io exception
value. This will usually be a Subscript, IO.ClosedStream, OS.SysErr,
or Fail exception (the last possible because of user-supplied readers
or writers), but the stream I/O module will rarely (perhaps never)
need to inspect it.

*)

signature STREAM_IO =
  sig
    type elem
    type vector

    type instream
    type outstream
    type out_pos

    type reader
    type writer
    type pos

    val input : instream -> vector * instream
    val input1 : instream -> (elem * instream) option
    val inputN : instream * int -> vector * instream
    val inputAll : instream -> vector * instream
    val canInput : instream * int -> int option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    val mkInstream : reader * vector -> instream
    val getReader : instream -> reader * vector
    val filePosIn : instream -> pos

    val setBufferMode : outstream * IO.buffer_mode -> unit
    val getBufferMode : outstream -> IO.buffer_mode

    val mkOutstream : writer * IO.buffer_mode -> outstream
    val getWriter : outstream -> writer * IO.buffer_mode
    val getPosOut : outstream -> out_pos
    val setPosOut : out_pos -> outstream
    val filePosOut : out_pos -> pos
  end

(**

[type elem]
[type vector]

The abstract types of stream elements and vectors of elements. For
text streams, these are Char.char and String.string, while for binary
streams, these are Word8.word and Word8Vector.vector.

[type instream] The type of buffered functional input streams.  Input
streams are in one of three states: active, truncated, or closed. When
initially created, the stream is active. When disconnected from its
underlying primitive reader (e.g., by getReader), the stream is
truncated. When closeIn is applied to the stream, the stream enters
the closed state. A closed stream is also truncated. The only real
difference between a truncated stream and a closed one is that in the
latter case, the stream's primitive I/O reader is closed.

Reading from a truncated input stream will never block; after all
buffered elements are read, input operations always return empty
vectors.

[type outstream] The type of buffered output streams. Unlike input
streams, these are imperative objects.  Output streams are in one of
three states: active, terminated, or closed. When initially created,
the stream is active. When disconnected from its underlying primitive
writer (e.g., by getWriter), the stream is terminated. When closeOut
is applied to the stream, the stream enters the closed state. A closed
stream is also terminated. The only real difference between a
terminated stream and a closed one is that in the latter case, the
stream's primitive I/O writer is closed.

In a terminated output stream, there is no mechanism for performing
more output, so any output operations will raise the IO.Io exception.

[type out_pos] The type of positions in output streams. This can be
used to reconstruct an output stream at the position recorded in the
out_pos value. Thus, the canonical representation for the type is
(outstream * pos).

[type reader]
[type writer]

The types of the readers and writers that underlie the input and
output streams.

[type pos] This is the type of positions in the underlying readers and
writers. In some instantiations of this signature (e.g.,
TextIO.StreamIO), pos is abstract; in others it may be concrete (e.g.,
Position.int in BinIO.StreamIO).

[input f] returns a vector of one or more elements from f and the
remainder of the stream, if any elements are available. If an
end-of-stream has been reached, then the empty vector is returned. The
function may block until one of these conditions is satisfied. This
function raises the Io exception if there is an error in the
underlying reader.

[input1 f] returns the next element in the stream f and the remainder
of the stream. If the stream is at the end, then NONE is returned. It
may block until one of these conditions is satisfied. This function
raises the Io exception if there is an error in the underlying reader.

[inputN (f, n)] returns a vector of the next n elements from f and the
rest of the stream. If fewer than n elements are available before the
next end-of-stream, it returns all of the elements up to that
end-of-stream. It may block until it can determine if additional
characters are available or an end-of-stream condition holds. This
function raises the Io exception if there is an error in the
underlying reader. It raises Size if n < 0 or the number of elements
to be returned is greater than maxLen. Also, inputN(f,0) returns
immediately with an empty vector and f, so this cannot be used as an
indication of end-of-stream.  Using instreams, one can synthesize a
non-blocking version of inputN from inputN and canInput, as inputN is
guaranteed not to block if a previous call to canInput returned
SOME(_).

[inputAll f] returns the vector of the rest of the elements in the
stream f (i.e., up to an end-of-stream), and a new stream f'. Care
should be taken when using this function, since it can block
indefinitely on interactive streams. This function raises the Io
exception if there is an error in the underlying reader. The stream f'
is immediately past the next end-of-stream of f. For ordinary files in
which only one end-of stream is expected, f' can be ignored. If a file
has multiple end-of-stream conditions (which can happen under some
operating systems), inputAll returns all the elements up to the next
end-of-stream. It raises Size if the number of elements to be returned
is greater than maxLen for the relevant vector type.

[canInput (f, n)] returns NONE if any attempt at input would block. It
returns SOME(k), where 0 <= k <= n, if a call to input would return
immediately with at least k characters. Note that k = 0 corresponds to
the stream being at end-of-stream.  Some streams may not support this
operation, in which case the Io exception will be raised. This
function also raises the Io exception if there is an error in the
underlying reader. It raises the Size exception if n < 0.

    Implementation note:

    It is suggested that implementations of canInput should attempt to
    return as large a k as possible. For example, if the buffer
    contains 10 characters and the user calls canInput (f, 15),
    canInput should call readVecNB(5) to see if an additional 5
    characters are available.

    Such a lookahead commits the stream to the characters read by
    readVecNB but it does not commit the stream to return those
    characters on the next call to input. Indeed, a typical
    implementation will simply return the remainder of the current
    buffer, in this case, consisting of 10 characters, if input is
    called. On the other hand, an implementation can decide to always
    respond to input with all the elements currently available,
    provided an earlier call to input has not committed the stream to
    a particular response. The only requirement is that any future
    call of input on the same input stream must return the same vector
    of elements.

[closeIn f] marks the stream closed, and closes the underlying
reader. Applying closeIn on a closed stream has no effect. This
function raises the Io exception if there is an error in the
underlying reader.

[endOfStream f] tests if f satisfies the end-of-stream condition. If
there is no further input in the stream, then this returns true;
otherwise it returns false. This function raises the Io exception if
there is an error in the underlying reader.  This function may block
when checking for more input. It is equivalent to

    (length(#1(input f)) = 0)

where length is the vector length operation. Note that even if
endOfStream returns true, subsequent input operations may succeed if
more data becomes available. A stream can have multiple end-of-streams
interspersed with normal elements. This can happen on Unix, for
example, if a user types control-D (#"\^D") on a terminal device, and
then keeps typing characters; it may also occur on file descriptors
connected to sockets.

Multiple end-of-streams is a property of the underlying reader. Thus,
readVec on a reader may return an empty string, then another call to
readVec on the same reader may return a nonempty string, then a third
call may return an empty string. It is always true, however, that

    endOfStream f = endOfStream f

In addition, if endOfStream f returns true, then input f returns
("",f') and endOfStream f' may or may not be true.

[output (f, vec)] writes the vector of elements vec to the stream
f. This raises the exception Io if f is terminated. This function also
raises the Io exception if there is an error in the underlying writer.

[output1 (f, el)] writes the element el to the stream f. This raises
the exception Io if f is terminated. This function also raises the Io
exception if there is an error in the underlying writer.

[flushOut f] flushes any output in f's buffer to the underlying
writer; it is a no-op on terminated streams. This function raises the
Io exception if there is an error in the underlying writer.

[closeOut f] flushes f's buffers, marks the stream closed, and closes
the underlying writer. This operation has no effect if f is already
closed. Note that if f is terminated, no flushing will occur. This
function raises the Io exception if there is an error in the
underlying writer or if flushing fails. In the latter case, the stream
is left open.

[mkInstream (rd, v)] returns a new instream built on top of the reader
rd with the initial buffer contents v.  If the reader does not
implement all of its fields (for example, if random access operations
are missing), then certain operations will raise exceptions when
applied to the resulting instream. The following table describes the
minimal relationship between instream operations and a reader:

    instream supports:    if reader implements:
    input, inputN, etc.	  readVec
    canInput              readVecNB
    endOfStream           readVec
    filePosIn             getPos and setPos

If the reader provides more operations, the resulting stream may use
them.  mkInstream should construct the input stream using the reader
provided. If the user wishes to employ synthesized functions in the
reader, the user may call mkInstream with an augmented reader
augmentReader(rd). See PRIM_IO for a description of the functions
generated by augmentReader.

Building more than one input stream on top of a single reader has
unpredictable effects, since readers are imperative objects. In
general, there should be a 1-1 correspondence between a reader and a
sequence of input streams. Also note that creating an input stream
this way means that the stream could be unaware that the reader has
been closed until the stream actually attempts to read from it.

[getReader f] marks the input stream f as truncated and returns the
underlying reader along with any unconsumed data from its buffer. The
data returned will have the value (closeIn f; inputAll f). The
function raises the exception Io if f is closed or truncated.

[filePosIn f] returns the primitive-level reader position that
corresponds to the next element to be read from the buffered stream
f. This raises the exception Io if the stream does not support the
operation, or if f has been truncated.  It should be true that, if
#1(inputAll f) returns vector v, then

    (setPos (filePosIn f); readVec (length v))

should also return v, assuming all operations are defined and terminate.

    Implementation note:

    If the pos type is a concrete integer corresponding to a byte
    offset, and the translation function (between bytes and elements)
    is known, the value can be computed directly. If not, the value is
    given by

    fun pos (bufp, n, r as RD rdr) = let
          val readVec = valOf (#readVec rdr)
          val getPos = valOf (#getPos rdr)
          val setPos = valOf (#setPos rdr)
          val savep = getPos()
          in
            setPos bufp;
            readVec n;
            getPos () before setPos savep
          end

    where bufp is the file position corresponding to the beginning of
    the current buffer, n is the number of elements already read from
    the current buffer, and r is the stream's underlying reader.


[setBufferMode (f, mode)]
[getBufferMode f]

These functions set and get the buffering mode of the output stream
f. Setting the buffer mode to IO.NO_BUF causes any buffered output to
be flushed. If the flushing fails, the Io exception is
raised. Switching the mode between IO.LINE_BUF and IO.BLOCK_BUF should
not cause flushing. If, in going from IO.BLOCK_BUF to IO.LINE_BUF, the
user desires that the buffer contain no newline characters, the user
should call flushOut explicitly.

[mkOutstream (wr, mode)] returns a new output stream built on top of
the writer wr with the indicated buffer mode.  If the writer does not
implement all of its fields (for example, if random access operations
are missing), then certain operations will raise exceptions when
applied to the resulting outstream. The following table describes the
minimal relationship between outstream operations and a writer:

    outstream supports:      if augmented writer implements:
    output, output1, etc.    writeArr
    flushOut                 writeArr
    setBufferMode            writeArr
    getPosOut                writeArr and getPos
    setPosOut                writeArr and setPos

If the writer provides more operations, the resulting stream may use
them.  mkOutstream should construct the output stream using the writer
provided. If the user wishes to employ synthesized functions in the
writer, the user may call mkOutstream with an augmented writer
augmentWriter(wr). See PRIM_IO for a description of the functions
generated by augmentWriter.

Building more than one outstream on top of a single writer has
unpredictable effects, since buffering may change the order of
output. In general, there should be a 1-1 correspondence between a
writer and an output stream. Also note that creating an output stream
this way means that the stream could be unaware that the writer has
been closed until the stream actually attempts to write to it.

[getWriter f] flushes the stream f, marks it as being terminated and
returns the underlying writer and the stream's buffer mode. This
raises the exception Io if f is closed, or if the flushing fails.

[getPosOut f] returns the current position of the stream f. This
raises the exception Io if the stream does not support the operation,
if any implicit flushing fails, or if f is terminated.

    Implementation note:

    A typical implementation of this function will require calculating
    a value of type pos, capturing where the next element written to f
    will be written in the underlying file. If the pos type is a
    concrete integer corresponding to a byte offset, and the
    translation function (between bytes and elements) is known, the
    value can be computed directly using getPos. If not, the value is
    given by

    fun pos (f, w as WR wtr) = let
          val getPos = valOf (#getPos wtr)
          in
            flushOut f;
            getPos ()
          end

    where f is the output stream and w is the stream's underlying writer.


[setPosOut opos] flushes the output buffer of the stream underlying
opos, sets the current position of the stream to the position recorded
in opos, and returns the stream. This can raise an Io exception if the
flushing fails, if the stream does not support the operation, or if
the stream underlying opos is terminated.

[filePosOut opos] returns the primitive-level writer position that
corresponds to the abstract output stream position opos.  Suppose we
are given an output stream f and a vector of elements v, and let opos
equal getPosOut(f). Then the code

    (setPos opos; writeVec{buf=v,i=0,sz=NONE})

should have the same effect as the last line of the function

    fun put (outs,x) = (flushOut outs;
                        output(outs,x);flushOut outs)

when called with (f,v) assuming all operations are defined and
terminate, and that the call to writeVec returns length v.

*)

signature STREAM_IO_EXTRA =
   sig
      include STREAM_IO
      type vector_slice

      structure Close:
         sig
            type t

            val close: t -> unit
            val equalsInstream: t * instream -> bool
            val make: instream -> t
         end

      val equalsIn: instream * instream -> bool
      val equalsOut: outstream * outstream -> bool
      val input1': instream -> elem option * instream
      val inputLine: instream -> (vector * instream) option
      val instreamReader: instream -> reader
      val mkInstream': {bufferContents: (bool * vector) option,
                        closed: bool,
                        reader: reader} -> instream
      val mkOutstream': {bufferMode: IO.buffer_mode,
                         closed: bool,
                         writer: writer} -> outstream
      val outputSlice: outstream * vector_slice -> unit
      val outstreamWriter: outstream -> writer
(*      val closedThing : string -> instream *)
   end

signature STREAM_IO_EXTRA_FILE =
   sig
      include STREAM_IO_EXTRA

      val inFd: instream -> Posix.IO.file_desc
      val mkInstream'': {bufferContents: (bool * vector) option,
                         closeAtExit: bool,
                         closed: bool,
                         reader: reader} -> instream
      val outFd: outstream -> Posix.IO.file_desc
      val mkOutstream'': {bufferMode: IO.buffer_mode,
                          closeAtExit: bool,
                          closed: bool,
                          writer: writer} -> outstream
  end
