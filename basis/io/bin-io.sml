(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BinIO : BIN_IO_EXTRA =
   ImperativeIOExtra
   (structure Array =
       struct
         open Word8Array
         fun rawArray i = array(i,0wx0)
       end
    structure ArraySlice = Word8ArraySlice
    structure PrimIO = BinPrimIO
    structure Vector = Word8Vector
    structure VectorSlice = Word8VectorSlice
    val chunkSize = Initial.TextIO.bufsize
    val fileTypeFlags = [Posix.FileSys.O.fromWord Initial.Posix_File_Sys.O.bin]
    val line = NONE
    val mkReader = Posix.IO.mkBinReader
    val mkWriter = Posix.IO.mkBinWriter
    val someElem = 0wx0: Word8.word
    val xlatePos = SOME {fromInt = fn i => i,
                         toInt = fn i => i})
