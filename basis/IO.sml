(*IO.sml*)

signature IO = sig
  exception Io of {name : string, function : string, cause : exn} 
  exception BlockingNotSupported
  exception NonblockingNotSupported
  exception RandomAccessNotSupported
  exception TerminatedStream
  exception ClosedStream
  datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
end; (*signature IO*)

structure IO : IO = struct
  exception Io of {name : string, function : string, cause : exn} 
  exception BlockingNotSupported
  exception NonblockingNotSupported
  exception RandomAccessNotSupported
  exception TerminatedStream
  exception ClosedStream
  datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
end; (*structure IO*)

(*
exception Io 
     is the principal exception raised when an error occurs in the I/O subsystem. The
     components of Io are: 
     name 
         The name component of the reader or writer. 
     function 
         The name of the STREAM_IO function raising the exception 
     cause 
         The underlying exception raised by the reader or writer, or detected at the stream
         I/O level. 

     Some of the standard causes are: 
         OS.SysErr if an actual system call was done and failed. 
         Subscript if ill-formed arguments are given. 
         BlockingNotSupported 
         NonblockingNotSupported 
         TerminatedStream 
         ClosedStream 
     The cause field of Io is not limited to these particular exceptions. Users who create their
     own readers or writers may raise any exception they like, which will be reported as the
     cause field of the resulting Io exception. 

exception BlockingNotSupported 
     used in the output, outputSubstr, output1 and flushOut I/O operations if the underlying
     writer does not support blocking writes; or in the input, inputN and input1 I/O operations
     if the underlying reader does not support blocking reads. It should never be raised within
     the I/O system; it should only be used in the cause field of an Io exception. 

exception NonblockingNotSupported 
     used by the canInput I/O operation if the underlying stream does not support
     non-blocking input. It should never be raised within the I/O system; it should only be
     used in the cause field of an Io exception. 

exception RandomAccessNotSupported 
     used by the STREAM_IO position operations to indicate that random access operations
     are not supported by the underlying device. 

exception TerminatedStream 
     used by the setPosIn I/O operation if the underlying stream is terminated. It should never
     be raised within the I/O system; it should only be used in the cause field of an Io
     exception. 

exception ClosedStream 
     used by the output I/O operations if the underlying stream is closed. This exception is
     actually raised by the reader or writer. It should never be raised within the I/O system; it
     should only be used in the cause field of an Io exception. 

datatype buffer_mode 
     specify the type of buffering used on output streams. 



Discussion

Lower-level imperative, stream and primitive I/O modules will never raise a bare
TerminatedStream, BlockingNotSupported, NonblockingNotSupported or ClosedStream
exception; these exceptions are only used in the cause field of the Io exception. However, any
module may raise Subscript directly if given ill-formed arguments, or may raise Io with
Subscript as the cause.
 *)
