signature BIN_IO =
  sig
    include IMPERATIVE_IO
      where type StreamIO.vector = Word8Vector.vector
      where type StreamIO.elem = Word8.word
      where type StreamIO.reader = BinPrimIO.reader
      where type StreamIO.writer = BinPrimIO.writer
      where type StreamIO.pos = BinPrimIO.pos
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
  end

signature BIN_IO_EXTRA =
   sig
      include BIN_IO

      val equalsIn: instream * instream -> bool
      val inFd: instream -> Posix.IO.file_desc
      val newIn: Posix.IO.file_desc * string -> instream
      val newOut: Posix.IO.file_desc * string -> outstream
      val outFd: outstream -> Posix.IO.file_desc
      val stdErr: outstream
      val stdIn: instream
      val stdOut: outstream
   end
