(*IO.sml*)

signature IO = sig
  exception Io of {name : string, function : string, cause : exn} 
  exception BlockingNotSupported
  exception NonblockingNotSupported
  exception RandomAccessNotSupported
  exception ClosedStream
  datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
end

structure IO : IO = struct
  exception Io of {name : string, function : string, cause : exn} 
  exception BlockingNotSupported
  exception NonblockingNotSupported
  exception RandomAccessNotSupported
  exception ClosedStream
  datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
end

(*
Description

exception Io of {
  name : string,
  function : string,
  cause : exn
}

    This is the principal exception raised when an error occurs in the
    I/O subsystem. The components of Io are:

    name
        The name component of the reader or writer. 
    function
        The name of the function raising the exception. 
    cause
        The underlying exception raised by the reader or writer, or
        detected at the stream I/O level.

    Some of the standard causes are:

        * OS.SysErr if an actual system call was done and failed.
        * Subscript if ill-formed arguments are given.
        * BlockingNotSupported
        * NonblockingNotSupported
        * ClosedStream 

    The cause field of Io is not limited to these particular
    exceptions. Users who create their own readers or writers may
    raise any exception they like, which will be reported as the cause
    field of the resulting Io exception.

exception BlockingNotSupported

    The exception used in the output, outputSubstr, output1, and
    flushOut I/O operations if the underlying writer does not support
    blocking writes; or in the input, inputN, and input1 I/O
    operations if the underlying reader does not support blocking
    reads. It should never be raised within the I/O system; it should
    only be used in the cause field of an Io exception.

exception NonblockingNotSupported

    The exception used by the canInput I/O operation if the underlying
    stream does not support non-blocking input. It should never be
    raised within the I/O system; it should only be used in the cause
    field of an Io exception.

exception RandomAccessNotSupported

    The exception used by the STREAM_IO position operations to
    indicate that random access operations are not supported by the
    underlying device. It should never be raised within the I/O
    system; it should only be used in the cause field of an Io
    exception.

exception ClosedStream

    This exception is used by the output I/O operations if the
    underlying object is closed or terminated. It should never be
    raised within the I/O system; it should only be used in the cause
    field of an Io exception.

datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF

    These values specify the type of buffering used on output
    streams. If an output stream has mode BLOCK_BUF, the
    implementation should store output in a buffer, actually writing
    the buffer's content to the device only when the buffer is
    full. If an output stream has mode NO_BUF, the implementation
    should write the argument bytes of any output function directly to
    the corresponding device. If an output stream has mode LINE_BUF,
    output bytes should be buffered until a newline character (#"\n")
    is seen, at which point the buffer should be flushed, including
    the newline character. For binary streams, LINE_BUF mode should be
    treated as a synonym for BLOCK_BUF.

        Implementation note:

        Output buffering is provided for efficiency, to reduce the
        number of writes to the underlying device, which may be an
        expensive operation. The I/O subsystem should select the
        initial buffer mode based on the output device. By default,
        output should be buffered. The optimum buffer size is
        specified by the chunkSize field in the underlying writer
        value. Output to TextIO.stdErr should be unbuffered. Output to
        a terminal-like device should be line-buffered. A simple test
        for this is

           OS.IO.kind iod = OS.IO.Kind.tty

        where iod is the I/O descriptor associated with the open
        stream.
*)
