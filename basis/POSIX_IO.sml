(** Posix IO operations.

The structure Posix.IO specifies functions that provide the primitive
POSIX input/output operations, as described in Section 6 of the POSIX
standard 1003.1,1996.

*)

signature POSIX_IO =
  sig
    eqtype file_desc
    eqtype pid

    datatype whence =
        SEEK_SET
      | SEEK_CUR
      | SEEK_END

    datatype open_mode =
        O_RDONLY
      | O_WRONLY
      | O_RDWR

    structure FD :
      sig
        include BIT_FLAGS
        val cloexec : flags
      end

    structure O :
      sig
        include BIT_FLAGS
        val append   : flags
        val nonblock : flags
        val sync     : flags
      end

    val close : file_desc -> unit
    val dup   : file_desc -> file_desc
    val dup2  : {old : file_desc, new : file_desc} -> unit
    val dupfd : {old : file_desc, base : file_desc} -> file_desc
    val pipe  : unit -> {infd : file_desc, outfd : file_desc}
    val getfd : file_desc -> FD.flags
    val setfd : file_desc * FD.flags -> unit

    datatype lock_type
      = F_RDLCK
      | F_WRLCK
      | F_UNLCK

    val setfl : file_desc * O.flags -> unit
    val getfl : file_desc -> O.flags * open_mode
    val lseek : file_desc * Position.int * whence -> Position.int

    val readVec  : file_desc * int -> Word8Vector.vector
    val readArr  : file_desc * Word8ArraySlice.slice -> int
    val writeVec : file_desc * Word8VectorSlice.slice -> int
    val writeArr : file_desc * Word8ArraySlice.slice -> int

    val mkTextReader : { fd : file_desc,
                         name : string,
                         initBlkMode : bool
                       } -> TextPrimIO.reader

    val mkTextWriter : { fd : file_desc,
                         name : string,
                         appendMode : bool,
                         initBlkMode : bool,
                         chunkSize : int
                       } -> TextPrimIO.writer

    val mkBinReader  : { fd : file_desc,
                         name : string,
                         initBlkMode : bool
                       } -> BinPrimIO.reader

    val mkBinWriter  : { fd : file_desc,
                         name : string,
                         appendMode : bool,
                         initBlkMode : bool,
                         chunkSize : int
                       } -> BinPrimIO.writer

  end

(**

[eqtype file_desc] Type of posix file descriptors.

[eqtype pid] Type of posix process ids.

[pipe()] This function creates a pipe (channel) and returns two file
descriptors that refer to the read (infd) and write (outfd) ends of
the pipe.

[dup fd] returns a new file descriptor that refers to the same open
file, with the same file pointer and access mode, as fd. The
underlying word (see Posix.FileSys.fdToWord) of the returned file
descriptor is the lowest one available. It is equivalent to dupfd
{old=fd, base=Posix.FileSys.wordToFD 0w0}.

[dup2 {old, new}] duplicates the open file descriptor old as file
descriptor new.

[close fd] closes the file descriptor fd.

[readVec (fd, n)] reads at most n bytes from the file referred to by
fd. The size of the resulting vector is the number of bytes that were
successfully read, which may be less than n. This function returns the
empty vector if end-of-stream is detected (or if n is 0). It raises
the Size exception if n < 0.

[readArr (fd, slice)] reads bytes from the file specified by fd into
the array slice slice and returns the number of bytes actually
read. The end-of-file condition is marked by returning 0, although 0
is also returned if the slice is empty. This function will raise
OS.SysErr if there is some problem with the underlying system call
(e.g., the file is closed).

[writeVec (fd, slice)]
[writeArr (fd, slice)]

These functions write the bytes the vector or array slice slice to the
open file fd. Both functions return the number bytes actually written
and will raise OS.SysErr if there is some problem with the underlying
system call (e.g., the file is closed or there is insufficient disk
space).

[structure FD] File descriptor flags.

    [cloexec] File descriptor flag that, if set, will cause the file
    descriptor to be closed should the opening process replace itself
    (through exec, etc.). If cloexec is not set, the open file
    descriptor will be inherited by the new process.

[structure O] File status flags.

    [append] File status flag which forces the file offset to be set
    to the end of the file prior to each write.

    [nonblock] File status flag used to enable non-blocking I/O.

    [sync] File status flag enabling writes using ``synchronized I/O
    file integrity completion.''

[datatype open_mode] Type specifying operations allowed on an open
file.

    O_RDONLY : Open a file for reading only.
    O_WRONLY : Open a file for writing only.
    O_RDWR   : Open a file for reading and writing.

[dupfd {old, base}] returns a new file descriptor bound to old. The
returned descriptor is greater than or equal to the file descriptor
base based on the underlying integer mapping defined by
Posix.FileSys.fdToWord and Posix.FileSys.wordToFD. It corresponds to
the POSIX fcntl function with the F_DUPFD command.

[getfd fd] gets the file descriptor flags associated with fd. It
corresponds to the POSIX fcntl function with the F_GETFD command.

[setfd (fd, fl)] sets the flags of file descriptor fd to fl. It
corresponds to the POSIX fcntl function with the F_SETFD command.

[getfl fd] gets the file status flags for the open file descriptor fd
and the access mode in which the file was opened. It corresponds to
the POSIX fcntl function with the F_GETFL command.

[setfl (fd, fl)] sets the file status flags for the open file
descriptor fd to fl. It corresponds to the POSIX fcntl function with
the F_SETFL command.

[lseek (fd, off, wh)] sets the file offset for the open file
descriptor fd to off if wh is SEEK_SET; to its current value plus off
bytes if wh is SEEK_CUR; or, to the size of the file plus off bytes if
wh is SEEK_END. Note that off may be negative.

[fsync fd] indicates that all data for the open file descriptor fd is
to be transferred to the device associated with the descriptor; it is
similar to a ``flush'' operation.

[datatype lock_type] Constructors of this type denote the lock
types. F_RDLCK indicates a shared or read lock. F_WRLCK indicates an
exclusive or write lock. F_WRLCK indicates a lock is unlocked or
inactive.

[structure FLock] File locks.

    [type flock] Type representing an advisory lock. It can be
    considered an abstraction of the record used as the argument to
    the flock function below.

    [flock {ltype, whence, start, len, pid}] creates a flock value
    described by the parameters. The whence and start parameters give
    the beginning file position as in lseek. The len value provides
    the number of bytes to be locked. If the section starts at the
    beginning of the file and len = 0, then the entire file is
    locked. Normally, pid will be NONE. This value is only used in a
    flock returned by getlk.

    [ltype flock]
    [whence flock]
    [start flock]
    [len flock]
    [pid flock]

    These are projection functions for the fields composing a flock
    value.

[getlk (fd, fl)] gets the first lock that blocks the lock description
fl on the open file descriptor fd. It corresponds to the POSIX fcntl
function with the F_GETLK command.

[setlk (fd, fl)] sets or clears a file segment lock according to the
lock description fl on the open file descriptor fd. An exception is
raised immediately if a shared or exclusive lock cannot be set. It
corresponds to the POSIX fcntl function with the F_SETLK command.

[setlkw (file_desc,flock)] This is similar to the setlk function above
except that setlkw waits on blocked locks until they are released. It
corresponds to the POSIX fcntl function with the F_SETLKW command.

[mkBinReader arg]
[mkTextReader arg]

These functions convert an open POSIX file descriptor into a
reader. From this, one can then construct an input stream. The
functions are comparable to the POSIX function fdopen.  The argument
fields have the following meanings:

    fd          : A file descriptor for a file opened for reading.
    name        : The name associated with the file, used in error
                  messages shown to the user.
    initBlkMode : False if the file is currently in non-blocking mode,
                  i.e., if the flag O.nonblock is set in #1(getfl fd).


[mkBinWriter arg]
[mkTextWriter arg]

These functions convert an open POSIX file descriptor into a
writer. From this, one can then construct an output stream. The
functions are comparable to the POSIX function fdopen.  The argument
fields have the following meanings:

    fd          : A file descriptor for a file opened for writing.
    name        : The name associated with the file, used in error
                  messages shown to the user.
    initBlkMode : False if the file is currently in non-blocking mode,
                  i.e., if the flag O.nonblock is set in #1(getfl fd).
    appendMode  : True if the file is in append mode, i.e., if the
                  flag O.append is set in #1(getfl fd).
    chunkSize   : The recommended size of write operations for
                  efficient writing.

*)
