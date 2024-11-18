signature POSIX_STAT = sig

  type mode
  type ino and dev and uid and gid
  type file_desc and iodesc
  structure ST : sig
    type stat
    val isDir  : stat -> bool
    val isChr  : stat -> bool
    val isBlk  : stat -> bool
    val isReg  : stat -> bool
    val isFIFO : stat -> bool
    val isLink : stat -> bool
    val isSock : stat -> bool
    val mode   : stat -> mode
    val ino    : stat -> ino
    val dev    : stat -> dev
    val nlink  : stat -> int
    val uid    : stat -> uid
    val gid    : stat -> gid
    val size   : stat -> Position.int
      (*  val atime : stat -> Time.time
        val mtime : stat -> Time.time
        val ctime : stat -> Time.time *)
  end

  val fstat : file_desc -> ST.stat

  val iodToFD : iodesc -> file_desc option
  val fdToIOD : file_desc -> iodesc
end

structure PosixStat : POSIX_STAT = struct

  val raiseSys = Initial2.raiseSys

  exception SysErr = Initial2.SysErr

  type mode = SysWord.word
  type ino = SysWord.word
  type dev = SysWord.word
  type uid = int
  type gid = int
  type file_desc = int
  type iodesc = int

  structure ST = struct
    type stat = int*int*int*int*int*int*int*int
    fun exBit j (i:int) = SysWord.andb(SysWord.fromInt i,SysWord.<<(0wx1,SysWord.fromInt j)) <> 0wx0
    fun isDir (s:stat) = exBit 5 (#1 s)
    fun isChr (s:stat) = exBit 4 (#1 s)
    fun isBlk (s:stat) = exBit 3 (#1 s)
    fun isReg (s:stat) = exBit 6 (#1 s)
    fun isFIFO (s:stat) = exBit 2 (#1 s)
    fun isLink (s:stat) = exBit 1 (#1 s)
    fun isSock (s:stat) = exBit 0 (#1 s)
    fun nlink (s:stat) = #5 s
    fun uid (s:stat) = #7 s
    fun gid (s:stat) = #8 s
    fun size (s:stat) = Position.fromInt (#6 s)
    fun dev (s:stat) = SysWord.fromInt (#4 s)
    fun ino (s:stat) = SysWord.fromInt (#3 s)
    fun mode (s:stat) = SysWord.fromInt (#2 s)
  end

  fun fstat (fd : file_desc) : ST.stat =
      let val res : ST.stat = prim("sml_fstat",fd)
      in if #1 res = ~1
         then raiseSys "Posix.FileSys.fstat" NONE ""
         else res
      end

  fun iodToFD (x:iodesc) : file_desc option = SOME x
  fun fdToIOD (x:file_desc) : iodesc = x

end
