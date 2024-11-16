(** Interface for I/O devices.

The OS.IO structure provides a general interface for polling I/O
devices. This interface has been modeled after the Unix SVR4 poll
interface. A poll_desc, created from an I/O descriptor, can be used to
test for various polling conditions.

*)

signature OS_IO =
  sig
    eqtype iodesc
    val hash : iodesc -> word
    val compare : iodesc * iodesc -> order
    eqtype iodesc_kind
    val kind : iodesc -> iodesc_kind

    structure Kind : sig
      val file    : iodesc_kind
      val dir     : iodesc_kind
      val symlink : iodesc_kind
      val tty     : iodesc_kind
      val pipe    : iodesc_kind
      val socket  : iodesc_kind
      val device  : iodesc_kind
    end

    eqtype poll_desc
    type poll_info
    exception Poll

    val pollDesc       : iodesc -> poll_desc option
    val pollToIODesc   : poll_desc -> iodesc
    val pollIn         : poll_desc -> poll_desc
    val pollOut        : poll_desc -> poll_desc
    val pollPri        : poll_desc -> poll_desc
    val poll           : poll_desc list * Time.time option -> poll_info list
    val isIn           : poll_info -> bool
    val isOut          : poll_info -> bool
    val isPri          : poll_info -> bool
    val infoToPollDesc : poll_info -> poll_desc
  end

(**

[eqtype iodesc] An iodesc is an abstraction for an opened OS object
that supports I/O (e.g., a file, console, or socket). In Unix, an
iodesc corresponds to a file descriptor, while in Microsoft Windows it
corresponds to a file handle.  Since iodesc values correspond to
low-level, OS-specific objects, they are not typically created
explicitly by the user, but are generated as a side-effect of the
creation of a more high-level abstraction. For example, TextIO.openIn
creates an instream value, from which the underlying PrimIO.reader can
be accessed. This latter value may contain the corresponding iodesc
value.

If the underlying operating system is known, there will usually be
mechanisms for converting between iodesc values and the type of value
used by the operating system. For example, the functions
Posix.FileSys.fdToIOD and Posix.FileSys.iodToFD provide this service
for POSIX implementations, translating between iodescs and open file
descriptors.

[hash iod] returns a hash value for the I/O descriptor iod.

Implementation note:

    hash must have the property that values produced are well
    distributed when taken modulo 2(n) for any n.

[compare (iod, iod')] returns LESS, EQUAL, or GREATER when iod is less
than, equal to, or greater than iod', respectively, in some underlying
linear ordering on iodesc values.

[eqtype iodesc_kind] This abstract type is used to represent the kind
of system object that an iodesc represents. The possible values are
defined in the Kind substructure.

[kind iod] returns the kind of system object that the I/O descriptor
iod represents. This will raise OS.SysErr if, for example, iod refers
to a closed file.

[structure Kind] These values represent the various kinds of system
objects that an I/O descriptor might represent. Note that a given
implementation may define other iodesc values not covered by these
definitions.

    [file] A regular file in the file system. The I/O descriptor
    associated with a stream produced by one of the BinIO or TextIO
    file opening operations will always have this kind.

    [dir] A directory in the file system. I/O descriptors associated
    with file system objects for which OS.FileSys.isDir returns true
    will have this kind.

    [symlink] A symbolic link or file system alias. I/O descriptors
    associated with file system objects for which OS.FileSys.isLink
    returns true will have this kind.

    [tty] A terminal console.

    [pipe] A pipe to another system process.

    [socket] A network socket.

    [device] A logical or physical hardware device.

[eqtype poll_desc] An abstract representation of a polling operation on an I/O
descriptor.

[type poll_info] An abstract representation of the per-descriptor information
returned by the poll operation.

[pollDesc iod] creates a polling operation on the given descriptor; NONE is
returned when no polling is supported by the I/O device.

[pollToIODesc pd] returns the I/O descriptor that is being polled using pd.

[pollIn pd]

[pollOut pd]

[pollPri pd]

These return a poll descriptor that has input (respectively, output,
high-priority) polling added to the poll descriptor pd. They raise Poll if input
(respectively, output, high-priority events) is not appropriate for the
underlying I/O device.

[poll (l, timeout)] polls a collection of I/O devices for the conditions
specified by the list of poll descriptors l. The argument timeout specifies the
timeout where:

    NONE means wait indefinitely.

    SOME(Time.zeroTime) means do not block.

    SOME(t) means timeout after time t.

The operation returns a list of poll_info values corresponding to those
descriptors in l whose conditions are enabled. The returned list respects the
order of the argument list, and a value in the returned list will reflect a
(nonempty) subset of the conditions specified in the corresponding argument
descriptor. The poll function will raise OS.SysErr if, for example, one of the
file descriptors refers to a closed file.

[isIn info]

[isOut info]

[isPri info]

These return true if input (respectively, output, priority information) is
present in info.

[infoToPollDesc pi] returns the underlying poll descriptor from poll information
pi.

*)
