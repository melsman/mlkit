(** OS file-system operations. *)

signature OS_FILE_SYS =
  sig
    type dirstream

    val openDir   : string -> dirstream
    val readDir   : dirstream -> string option
    val rewindDir : dirstream -> unit
    val closeDir  : dirstream -> unit

    val chDir     : string -> unit
    val getDir    : unit -> string
    val mkDir     : string -> unit
    val rmDir     : string -> unit
    val isDir     : string -> bool
    val isLink    : string -> bool
    val readLink  : string -> string
    val fullPath  : string -> string
    val realPath  : string -> string
    val modTime   : string -> Time.time
    val fileSize  : string -> Position.int
    val setTime   : string * Time.time option -> unit
    val remove    : string -> unit
    val rename    : {old : string, new : string} -> unit

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access    : string * access_mode list -> bool
    val tmpName   : unit -> string

    eqtype file_id

    val fileId    : string -> file_id
    val hash      : file_id -> word
    val compare   : file_id * file_id -> order
  end

(**

[type dirstream] Type of directory streams.

[openDir path] opens the directory specified by path and returns a
directory stream for use with readDir, rewindDir, and closeDir. The
stream reads the directory entries off the file system in some
unspecified order. It raises SysErr if, for example, the directory
does not exist or is not accessible.

[readDir dir] returns and removes one filename from the directory
stream dir. When the directory stream is empty (that is, when all
entries have been read from the stream), NONE is returned. readDir
filters out the names corresponding to the current and parent arcs.

[rewindDir dir] resets the directory stream dir, as if it had just
been opened. It raises SysErr in case of an operating system error,
though, since the directory stream has already been opened, an error
should not be likely.

[closeDir dir] closes the directory stream dir, releasing any system
resources associated with it. Any subsequent read or rewind on the
stream will raise exception SysErr. Closing a closed directory stream,
however, has no effect.

[chDir s] changes the current working directory to s. This affects
future calls to all functions that access the file system. These
include the input/output functions such as TextIO.openIn and
TextIO.openOut, and functions defined in this structure. It raises
SysErr if, for example, the directory does not exist or is not
readable.

The chDir function will also change the current volume (on systems
with volumes) if one is specified. This function does not allow the
user to change the current working directory of another volume than
the current volume, even on systems where this concept is otherwise
supported.

[getDir ()] returns an absolute canonical pathname of the current
working directory. This includes the current volume for systems
supporting volumes.

[mkDir s] creates a directory s on the file system. If s has multiple
arcs, each of the ancestor directories will need to be created first,
if it does not already exist. mkDir raises SysErr if, for example, the
directory in which s is to be created does not exist or is not
writable.

[rmDir s] removes directory s from the file system. It raises SysErr
if, for example, s does not exist or if the directory in which s
resides is not writable, or if the directory is not empty.

[isDir s] tests whether s is a directory. It raises SysErr if, for
example, s does not exist or if the directory in which s resides is
not accessible.

[isLink s] returns true if s names a symbolic link. It raises SysErr
if, for example, s does not exist or there is an access violation. On
operating systems without symbolic links, it will always return false
unless an exception is raised first.

[readLink s] returns the contents of the symbolic link s. It raises
SysErr if, for example, s does not exist or is not a symbolic link, or
there is an access violation. On operating systems without symbolic
links, it raises SysErr unconditionally.

The precise form of the returned string, in particular, whether it
corresponds to an absolute or relative path, is system-dependent.

[fullPath path] returns an absolute canonical path that names the same
file system object as path. The resulting path will have a volume
prefix (on systems supporting volumes), all occurrences of the
current, parent, and empty arcs will have been expanded or removed,
and any symbolic links will have been fully expanded. An empty path is
treated as ".". It raises SysErr if, for example, a directory on the
path, or the file or directory named, does not exist or is not
accessible or if there is a link loop.

[realPath path] returns a canonical path that names the same file
system object as path. If path is an absolute path, then realPath acts
like fullPath. If path is relative and on the same volume as the
current working directory, then it returns a path that is relative to
the current working directory, but in which the symbolic links have
been expanded. Otherwise, it raises OS.Path.Path.

Implementation note:

    This function can be implemented as follows:

            fun realPath p = if OS.Path.isAbsolute p
                  then fullPath p
                  else OS.Path.mkRelative{
                      path=fullPath p, relativeTo=fullPath(getDir())
                    }

[modTime path] returns the modification time of file path. It raises
SysErr if, for example, path does not exist or if the directory in
which path resides is not accessible.

[fileSize path] returns the size of file path in bytes. It raises
SysErr if, for example, path does not exist or if the directory in
which path resides is not accessible.

[setTime (path, opt)] sets the modification and access time of file
path. If opt is SOME(t), then the time t is used; otherwise the
current time (i.e., Time.now()) is used.  It raises SysErr if path
does not exist, the directory in which path resides is not accessible,
or the user does not have the appropriate permission.

[remove path] deletes the file path from the file system. It raises
SysErr if, path does not exist or is not writable, if the directory in
which path resides is not writable, or file is a directory.  Use the
rmDir function to delete directories.

If one removes a file that has been opened for reading or writing, the
behavior of subsequent reads and writes is undefined. For example,
removing the file may close all existing streams or generate an
exception. The Unix idiom of opening a file and then removing it is
not portable.

[rename {old, new}] changes the name of file old to new. If new and
old refer to the same file, rename does nothing. If a file called new
exists, it is removed. It raises SysErr if, for example, old does not
exist, or if one of the directories in which old or new reside is not
writable. This may also fail if old refers to an open file, or if old
and new are on different file systems, i.e., if a copy is required.

[datatype access_mode] Access mode specified using the constructors
A_READ, A_WRITE, and A_EXEC.

[access (path, accs)] tests the access permissions of file path,
expanding symbolic links as necessary. If the list accs of required
access modes is empty, it tests whether path exists. If accs contains
A_READ, A_WRITE, or A_EXEC, respectively, it tests whether the user
process has read, write, or execute permission for the file, testing
their conjunction if more than one are present.  Note that access is
also implicitly testing the user's access to the parent directories of
the file. The function will only raise OS.SysErr for errors unrelated
to resolving the pathname and the related permissions, such as being
interrupted by a signal during the system call.

Implementation note:

    On systems that do not support a notion of execution permissions,
    the access should accept but ignore the A_EXEC value.

[tmpName()] creates a new empty file with a unique name and returns
the full pathname of the file. The named file will be readable and
writable by the creating process, but, if the host operating systems
supports it, not accessible by other users. This function can be used
to create a temporary file that will not collide with other
applications.

This function raises SysErr if it cannot create the unique file or
filename.

[eqtype file_id] Type representing unique identifiers to file system
objects. A value of this type is not persistent across changes in the
file system (e.g., mount/unmount) but it is better than pathnames for
uniquely identifying files. A file_id value should not be confused
with the open file identifier OS.IO.iodesc.

[fileId path] returns the unique file_id value associated with the
file system object designated by the pathname path. In particular, if
fileId p = fileId p', then the paths p and p' refer to the same file
system object. Note that if p is a symbolic link, then fileId p =
fileId(readLink p) .

[hash fid] returns a hash value associated with fid.

Implementation note:

    hash must have the property that values produced are well
    distributed when taken modulo 2(n) for any n.

[compare (fid, fid')] returns LESS, EQUAL, or GREATER when fid is less
than, equal to, or greater than fid', respectively, in some underlying
linear ordering on file_id values.

[Discussion]

For functions dealing with file attributes, such as fileSize or
rename, the arguments can be directories as well as ordinary files.

*)
