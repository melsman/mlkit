signature POSIX_IO = 
  sig
    eqtype file_desc
    eqtype pid
(*    eqtype open_mode *)

    datatype whence
      = SEEK_SET
      | SEEK_CUR
      | SEEK_END

    datatype open_mode =
        O_RDONLY
      | O_WRONLY
      | O_RDWR

    structure O :
      sig
        include BIT_FLAGS
        val append : flags
        val nonblock : flags
        val sync : flags
      end

    val close : file_desc -> unit
    val dup : file_desc -> file_desc
    val dup2 : {old : file_desc, new : file_desc} -> unit
    val dupfd : {old : file_desc, base : file_desc} -> file_desc
    val pipe : unit -> {infd : file_desc, outfd : file_desc}
   
    datatype lock_type
      = F_RDLCK
      | F_WRLCK
      | F_UNLCK

    val setfl : file_desc * O.flags -> unit
    val getfl : file_desc -> O.flags * open_mode

    val lseek : file_desc * Position.int * whence
                  -> Position.int

    val readVec : file_desc * int -> Word8Vector.vector
    val readArr : file_desc * Word8ArraySlice.slice -> int
    val writeVec : file_desc * Word8VectorSlice.slice -> int
    val writeArr : file_desc * Word8ArraySlice.slice -> int

    val mkTextReader : {
                           fd : file_desc,
                           name : string,
                           initBlkMode : bool
                         } -> TextPrimIO.reader

    val mkTextWriter : {
                           fd : file_desc,
                           name : string,
                           appendMode : bool,
                           initBlkMode : bool,
                           chunkSize : int
                         } -> TextPrimIO.writer

    val mkBinReader  : {
                           fd : file_desc,
                           name : string,
                           initBlkMode : bool
                         } -> BinPrimIO.reader

    val mkBinWriter  : {
                           fd : file_desc,
                           name : string,
                           appendMode : bool,
                           initBlkMode : bool,
                           chunkSize : int
                         } -> BinPrimIO.writer

  end
