(** POSIX file system operations.

The structure Posix.FileSys provides access to file system operations
as described in Section 5 of the POSIX standard 1003.1,1996.

*)

signature POSIX_FILE_SYS =
  sig
    eqtype uid
    eqtype gid
    eqtype file_desc

    val fdToWord  : file_desc -> SysWord.word
    val wordToFD  : SysWord.word -> file_desc
    val fdToIOD   : file_desc -> OS.IO.iodesc
    val iodToFD   : OS.IO.iodesc -> file_desc option

    type dirstream

    val opendir   : string -> dirstream
    val readdir   : dirstream -> string option
    val rewinddir : dirstream -> unit
    val closedir  : dirstream -> unit
    val chdir     : string -> unit
    val getcwd    : unit -> string
    val stdin     : file_desc
    val stdout    : file_desc
    val stderr    : file_desc

    structure S :
      sig
        eqtype mode
        include BIT_FLAGS where type flags = mode
        val irwxu : mode
        val irusr : mode
        val iwusr : mode
        val ixusr : mode
        val irwxg : mode
        val irgrp : mode
        val iwgrp : mode
        val ixgrp : mode
        val irwxo : mode
        val iroth : mode
        val iwoth : mode
        val ixoth : mode
        val isuid : mode
        val isgid : mode
      end

    structure FD :
      sig
        include BIT_FLAGS
        val cloexec : flags
      end

    structure O :
      sig
        include BIT_FLAGS
        val append   : flags
        val excl     : flags
        val noctty   : flags
        val nonblock : flags
        val sync     : flags
        val trunc    : flags
      end

    datatype open_mode =
        O_RDONLY
      | O_WRONLY
      | O_RDWR

    val openf     : string * open_mode * O.flags -> file_desc
    val createf   : string * open_mode * O.flags * S.mode -> file_desc
    val creat     : string * S.mode -> file_desc
    val umask     : S.mode -> S.mode
    val link      : {old : string, new : string} -> unit
    val mkdir     : string * S.mode -> unit
    val mkfifo    : string * S.mode -> unit
    val unlink    : string -> unit
    val rmdir     : string -> unit
    val rename    : {old : string, new : string} -> unit
    val symlink   : {old : string, new : string} -> unit
    val readlink  : string -> string

    eqtype dev
    val wordToDev : SysWord.word -> dev
    val devToWord : dev -> SysWord.word

    eqtype ino
    val wordToIno : SysWord.word -> ino
    val inoToWord : ino -> SysWord.word

    structure ST :
      sig
        type stat
        val isDir  : stat -> bool
        val isChr  : stat -> bool
        val isBlk  : stat -> bool
        val isReg  : stat -> bool
        val isFIFO : stat -> bool
        val isLink : stat -> bool
        val isSock : stat -> bool
        val mode   : stat -> S.mode
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

    val stat  : string -> ST.stat
    val lstat : string -> ST.stat
    val fstat : file_desc -> ST.stat

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access    : string * access_mode list -> bool
    val chmod     : string * S.mode -> unit
    val fchmod    : file_desc * S.mode -> unit
    val chown     : string * uid * gid -> unit
    val fchown    : file_desc * uid * gid -> unit
  (*  val utime : string
                   * {actime : Time.time, modtime : Time.time} option
                   -> unit *)
    val ftruncate : file_desc * Position.int -> unit
    val pathconf  : string * string -> SysWord.word option
    val fpathconf : file_desc * string -> SysWord.word option
  end

(**

[eqtype uid] The type of a user identifier; identical to
Posix.ProcEnv.uid.

[eqtype gid] The type of a group identifier; identical to
Posix.ProcEnv.gid.

[eqtype file_desc] The type of an open file descriptor.

[fdToWord fd]
[wordToFD w]

These functions convert between an abstract open file descriptor and
the integer representation used by the operating system. These calls
should be avoided where possible, for the SML implementation may be
able to garbage collect (i.e., automatically close) any file_desc
value that is not accessible, but it cannot do this for any file_desc
that has ever been made concrete by fdToWord. Also, there is no
validation that the file descriptor created by wordToFD corresponds to
an actually open file.

[fdToIOD fd]
[iodToFD iod]

These functions convert between a POSIX open file descriptor and the
handle used by the OS subsystem. The function iodToFD returns an
option type because, on certain systems, some open I/O devices are not
associated with an underlying open file descriptor.

[type dirstream] The type of a directory stream opened for reading. A
directory stream is an ordered sequence of all the directory entries
in a particular directory. This type is identical to
OS.FileSys.dirstream.

[opendir dirName] opens the directory designated by the dirName
parameter and associates a directory stream with it. The directory
stream is positioned at the first entry.

[readdir dir] returns and removes one filename from the directory
stream dir. When the directory stream is empty (that is, when all
entries have been read from the stream), NONE is returned. Entries for
"." (current directory) and ".." (parent directory) are never
returned.

    Rationale: The reason for filtering out the current and parent
    directory entries is that it makes recursive walks of a directory
    tree easier.

[rewinddir d] repositions the directory stream d for reading at the
beginning.

[closedir d] closes the directory stream d. Closing a previously
closed dirstream does not raise an exception.

[chdir s] changes the current working directory to s.

[getcwd()] Returns the absolute pathname of the current working
directory.

[stdin]
[stdout]
[stderr]

The standard input, output, and error file descriptors.

[structure S] The structure Posix.FileSys.S contains file modes.

    [eqtype mode] A file mode is a set of (read, write, execute)
    permissions for the owner of the file, members of the file's
    group, and others.

    [irwxu] Read, write, and execute permission for ``user'' (the
    file's owner).

    [irusr] Read permission for ``user'' (the file's owner).

    [iwusr] Write permission for ``user'' (the file's owner).

    [ixusr] Execute permission for ``user'' (the file's owner).

    [irwxg] Read, write, and execute permission for members of the
    file's group.

    [irgrp] Read permission for members of the file's group.

    [iwgrp] Write permission for members of the file's group.

    [ixgrp] Execute permission for members of the file's group.

    [irwxo] Read, write, and execute permission for ``others'' (all
    users).

    [iroth] Read permission for ``others'' (all users).

    [iwoth] Write permission for ``others'' (all users).

    [ixoth] Execute permission for ``others'' (all users).

    [isuid] Set-user-id mode, indicating that the effective user ID of
    any user executing the file should be made the same as that of the
    owner of the file.

    [isgid] Set-group-id mode, indicating that the effective group ID
    of any user executing the file should be made the same as the
    group of the file.

[structure O] The structure Posix.FileSys.O contains file status flags
used in calls to openf.

    [append] If set, the file pointer is set to the end of the file
    prior to each write.

    [excl] This flag causes the open to fail if the file already
    exists.

    [noctty] If the path parameter identifies a terminal device, this
    flag assures that the terminal device does not become the
    controlling terminal for the process.

    [nonblock] Open, read, and write operations on the file will be
    nonblocking.

    [sync] If set, updates and writes to regular files and block
    devices are synchronous updates. On return from a function that
    performs a synchronous update (writeVec, writeArr, ftruncate,
    openf with trunc), the calling process is assured that all data
    for the file has been written to permanent storage, even if the
    file is also open for deferred update.

    [trunc] This causes the file to be truncated (to zero length) upon
    opening.

[datatype open_mode] Operations allowed on an open file.

    O_RDONLY : Open a file for reading only.
    O_WRONLY : Open a file for writing only.
    O_RDWR   : Open a file for reading and writing.

[openf (s, om, f)]
[createf (s, om, f, m)]

These calls open a file named s for reading, writing, or both
(depending on the open mode om). The flags f specify the state of the
open file. If the file does not exist, openf raises the OS.SysErr
exception whereas createf creates the file, setting its protection
mode to m (as modified by the umask).

Note that, in C, the roles of openf and createf are combined in the
function open. The first acts like open without the O_CREAT flag; the
second acts like open with the O_CREAT flag and the specified
permission mode. Also, the createf function should not be confused
with the creat function below, which behaves like its C namesake.

[creat (s, m)] opens a file s for writing. If the file exists, this
call truncates the file to zero length. If the file does not exist, it
creates the file, setting its protection mode to m (as modified by the
umask). This is equivalent to the expression:
createf(s,O_WRONLY,O.trunc,m)

[umask cmask] sets the file mode creation mask of the process to cmask
and returns the previous value of the mask.  Whenever a file is
created (by openf, creat, mkdir, etc.), all file permission set in the
file mode creation mask are removed from the mode of the created
file. This clearing allows users to restrict the default access to
their files.

The mask is inherited by child processes.

[link {old, new}] creates an additional hard link (directory entry)
for an existing file. Both the old and the new link share equal access
rights to the underlying object.  Both old and new must reside on the
same file system. A hard link to a directory cannot be created.

Upon successful completion, link updates the file status change time
of the old file, and updates the file status change and modification
times of the directory containing the new entry. (See
Posix.FileSys.ST.)

[mkdir (s, m)] creates a new directory named s with protection mode m
(as modified by the umask).

[mkfifo (s, m)] makes a FIFO special file (or named pipe) s, with
protection mode m (as modified by the umask).

[unlink path] removes the directory entry specified by path and, if
the entry is a hard link, decrements the link count of the file
referenced by the link.  When all links to a file are removed and no
process has the file open or mapped, all resources associated with the
file are reclaimed, and the file is no longer accessible. If one or
more processes have the file open or mapped when the last link is
removed, the link is removed before unlink returns, but the removal of
the file contents is postponed until all open or map references to the
file are removed. If the path parameter names a symbolic link, the
symbolic link itself is removed.

[rmdir s] removes a directory s, which must be empty.

[rename {old, new}] changes the name of a file system object from old
to new.

[symlink {old, new}] creates a symbolic link new. Any component of a
pathname resolving to new will be replaced by the text old. Note that
old may be a relative or absolute pathname, and might not be the
pathname of any existing file.

[readlink s] reads the value of a symbolic link s.

[eqtype dev] The type of a device identifier. The device identifier
and the file serial number (inode or ino) uniquely identify a file.

[wordToDev w]
[devToWord dev]

These functions convert between dev values and words by which the
operating system identifies a device. There is no verification that a
value created by wordToDev corresponds to a to a valid device
identifier.

[eqtype ino] The type of a file serial number (inode).

[wordToIno w]
[inoToWord ino]

These functions convert between ino values and words by which the
operating system identifies an inode. There is no verification that a
value created by wordToIno corresponds to a to a valid inode.

[structure ST] The structure Posix.FileSys.ST contains file status
operations.

    [type stat] This type models status information concerning a file.

    [isDir stat]
    [isChr stat]
    [isBlk stat]
    [isReg stat]
    [isFIFO stat]
    [isLink stat]
    [isSock stat]

    These functions return true if the file described by the parameter
    is, respectively, a directory, a character special device, a block
    special device, a regular file, a FIFO, a symbolic link, or a
    socket.

    [mode st] returns the protection mode of the file described by st.

    [ino stat]
    [dev stat]

    These functions return the file serial number (inode) and the
    device identifier, respectively, of the corresponding file.

    [nlink st] returns the number of hard links to the file described
    by st.

    [uid stat]
    [gid stat]

    These functions return the owner and group ID of the file.

    [size st] returns the size (number of bytes) of the file described
    by st.

    [atime stat]
    [mtime stat]
    [ctime stat]

    These functions return, respectively, the last access time, the
    last modification time or the last status change time of the file.

[stat string]
[lstat string]
[fstat file_desc]

These functions return information on a file system object. For stat
and lstat, the object is specified by its pathname. Note that an empty
string causes an exception. For fstat, an open file descriptor is
supplied.  lstat differs from stat in that, if the pathname argument
is a symbolic link, the information concerns the link itself, not the
file to which the link points.

[datatype access_mode] This type is identical to
OS.FileSys.access_mode.

[access (s, l)] checks for accessibility of file s. If l is the empty
list, it checks for the existence of the file; if l contains A_READ,
it checks for the readability of s based on the real user and group
IDs of the process; and so on.  The value returned depends only the
appropriate privileges of the process and the permissions of the
file. A directory may be indicated as writable by access, but an
attempt to open it for writing will fail (although files may be
created there). A file's permissions may indicate that it is
executable, but the exec can fail if the file is not in the proper
format. Conversely, if the process has appropriate privileges, access
will return true if none of the appropriate file permissions are set.

[chmod (s, mode)] changes the permissions of s to mode.

[fchmod (fd, mode)] changes the permissions of the file opened as fd
to mode.

[chown (s, uid, gid)] changes the owner and group of file s to uid and
gid, respectively.

[fchown (fd, uid, gid)] changes the owner and group of the file opened
as fd to uid and gid, respectively.

[utime (f, SOME{actime,modtime})] sets the access and modification
times of the file f to actime and modtime, respectively.

[utime (f, NONE)] sets the access and modification times of a file to
the current time.

[ftruncate (fd, n)] changes the length of a file opened as fd to n
bytes. If the new length is less than the previous length, all data
beyond n bytes is discarded. If the new length is greater than the
previous length, the file is extended to its new length by the
necessary number of zero bytes.

[pathconf (s, p)]
[fpathconf (fd, p)]

These functions return the value of property p of the file system
underlying the file specified by s or fd. For integer-valued
properties, if the value is unbounded, NONE is returned. If the value
is bounded, SOME(v) is returned, where v is the value. For
boolean-value properties, if the value is true, SOME(1) is returned;
otherwise, SOME(0) or NONE is returned. The OS.SysErr exception is
raised if something goes wrong, including when p is not a valid
property or when the implementation does not associate the property
with the file.  In the case of pathconf, read, write, or execute
permission of the named file is not required, but all directories in
the path leading to the file must be searchable.

The properties required by POSIX are described below. A given
implementation may support additional properties.

    "CHOWN_RESTRICTED" True if the use of chown on any files (other
                       than directories) in the specified directory is
                       restricted to processes with appropriate
                       privileges. This property only applies to
                       directories.

    "LINK_MAX"         The maximum value of a file's link count as
                       returned by the ST.nlink function.

    "MAX_CANON"        The maximum number of bytes that can be stored
                       in an input queue. This property only applies
                       to terminal devices.

    "MAX_INPUT"        The maximum number of bytes allowed in an input
                       queue before being read by a process. This
                       property only applies to terminal devices.

    "NAME_MAX"         The maximum number of bytes in a filename. This
                       value may be as small as 13, but is never
                       larger than 255. This property only applies to
                       directories and its value applies to filenames
                       within the directory.

    "NO_TRUNC"         True if supplying a filename longer than
                       allowed by "NAME_MAX" causes an error; false if
                       long filenames are truncated. This property
                       only applies to directories.

    "PATH_MAX"         The maximum number of bytes in a pathname. This
                       value is never larger than 65,535 and is the
                       maximum length of a relative pathname when the
                       specified directory is the working
                       directory. This property only applies to
                       directories.

    "PIPE_BUF"         Maximum number of bytes guaranteed to be
                       written atomically. This is applicable only to
                       a FIFO. The value returned applies to the
                       referenced object. If the path or file
                       descriptor parameter refers to a directory, the
                       value returned applies to any FIFO that exists
                       or can be created within the directory.

    "VDISABLE"         If defined, the integer code ord(c) of the
                       character c which can be used to disable the
                       terminal special characters specified in
                       Posix.TTY.V. This property only applies to
                       terminal devices.

    "ASYNC_IO"         True if asynchronous input or output operations
                       may be performed on the file.

    "SYNC_IO"          True if synchronous input or output operations
                       may be performed on the file.

    "PRIO_IO"          True if prioritized input or output operations
                       may be performed on the file.

Implementation note: An implementation can call the operating system's
pathconf or fpathconf functions, which return an integer. If the
returned value is -1 and errno has been set, an exception is
raised. Otherwise, a returned value of -1 should be mapped to NONE,
and other values should be wrapped in SOME and returned.

[Discussion]

The encoding of boolean values as int option, with false having two
values, is an unpleasant choice. It would be preferable to split these
two functions into four, with one pair handling integer-valued
properties, with the present return type, and the other pair handling
boolean-valued properties, returning values of type
bool. Unfortunately, the nature of the POSIX pathconf and fpathconf
functions would make this a nightmare for the implementor.

First, the specification of these functions provides a non-negative
integer return value for both booleans and numbers. System include
files provide no inherent information as to the type of a
property. Although the basic properties specified by POSIX have fixed
types, each system is allowed to add its own non-standard
properties. Thus, for an SML implementation to make the distinction,
it would have to rely on somehow gleaning the information from, e.g.,
system-specific manual pages.

In addition, the POSIX specification is unclear on how boolean values
are encoded. Some systems return 0 for false; others appear to return
-1 without setting errno. Technically, the latter value may be
interpreted as meaning that the property value is unknown or
unspecified. From the programmer's point of view, this means that the
property is not usable.

This situation probably precludes automatically generating these
functions on a per system basis. Given this, the current return types
and values appear to be the only reasonable choice.

*)
