(** Primitive IO operations.

The PRIM_IO signature is an abstraction of the fundamental system call
operations commonly available on open file descriptors
OS.IO.iodesc. Imperative and Stream I/O facilities do not access the
operating system directly, but access the appropriate Primitive I/O
reader and writer that accomplishes the required system call.

Several operations in the PRIM_IO interface will raise exceptions that
have been left intentionally unspecified. The actual exception raised
will usually be operating system-dependent, but may vary. For example,
a reader connected to a prime number generator might raise
Overflow. More typically, the close operation on a reader or writer
may cause an exception to be raised if there is a failure in the
underlying file system, such as the disk being full or the file server
being unavailable. In addition, one would expect readVec and readVecNB
to raise Size if the resulting vector would exceed the maximum allowed
vector size or if the input parameter is negative. Similarly, one
would expect readArr, readArrNB, writeArr, writeArrNB, writeVec, and
writeVecNB to raise Subscript if array bounds are violated. Readers
and writers should not, in general, raise the IO.Io exception. It is
assumed that the higher levels will catch the exceptions raised at the
Primitive I/O level, and appropriately construct and raise the IO.Io
exception.

A reader is required to raise IO.Io if any of its functions, except
close or getPos, is invoked after a call to close. A writer is
required to raise IO.Io if any of its functions, except close, is
invoked after a call to close. In both cases, the cause field of the
exception should be IO.ClosedStream.

*)

signature PRIM_IO =
  sig
    type elem
    type vector
    type vector_slice
    type array
    type array_slice

    eqtype pos

    val compare : pos * pos -> order

    datatype reader
      = RD of {
	name : string,
	chunkSize : int,
	readVec : (int -> vector) option,
	readArr : (array_slice -> int) option,
	readVecNB : (int -> vector option) option,
	readArrNB : (array_slice -> int option) option,
	block : (unit -> unit) option,
	canInput : (unit -> bool) option,
	avail : unit -> int option,
	getPos : (unit -> pos) option,
	setPos : (pos -> unit) option,
	endPos : (unit -> pos) option,
	verifyPos : (unit -> pos) option,
	close : unit -> unit,
	ioDesc : OS.IO.iodesc option
      }

    datatype writer
      = WR of {
	name : string,
	chunkSize : int,
	writeVec : (vector_slice -> int) option,
	writeArr : (array_slice -> int) option,
	writeVecNB : (vector_slice -> int option) option,
	writeArrNB : (array_slice -> int option) option,
	block : (unit -> unit) option,
	canOutput : (unit -> bool) option,
	getPos : (unit -> pos) option,
	setPos : (pos -> unit) option,
	endPos : (unit -> pos) option,
	verifyPos : (unit -> pos) option,
	close : unit -> unit,
	ioDesc : OS.IO.iodesc option
      }
    val openVector : vector -> reader
    val nullRd : unit -> reader
    val nullWr : unit -> writer

    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer
  end

(**

[type elem] The elem type is an abstraction that represents the
``element'' of a file (or device, etc.). Typically, elements are
either characters (char) or bytes (Word8.word).

[type vector]
[type vector_slice]
[type array]
[type array_slice]

One typically reads or writes a sequence of elements in one
operation. The vector type is an immutable vector of elements; the
vector_slice type is a slice of a vector; the array type is an mutable
array of elements; and the array_slice type is a slice of an array.

[eqtype pos] This is an abstraction of a position in a file, usually
used for random access.

[compare (pos, pos')] returns LESS, EQUAL, or GREATER when pos is less
than, equal to, or greater than pos', respectively, in some underlying
linear ordering on pos values.

[datatype reader] A reader is an abstraction for a source of items of
type elem. Usually, it will correspond to a file or device opened for
reading. It can also represent the output of some algorithm or
function, not necessarily connected to the outside world, that
produces elements. The resulting sequence of elements is potentially
unbounded. In the description below, we will usually refer to the
limit sequence as a ``file,'' as this is the most common instance.

    name

        is the name associated with this reader, used in error
        messages shown to the user.

    chunkSize

        is the recommended (efficient) size of read operations on this
        reader. This is typically set to the block size of the
        operating system's buffers. chunkSize = 1 strongly recommends
        unbuffered reads, but since buffering is handled at a higher
        level, it cannot guarantee it. chunkSize <= 0 is illegal.

    readVec(n)

        when present, reads upto n elements returning a vector of the
        elements read. This function returns the empty vector if
        end-of-stream is detected (or if n is 0). It blocks, if
        necessary, until end-of-stream is detected or at least one
        element is available.

        It is recommended that implementations of this function raise
        the Size exception if n < 0.

    readArr(slice)

        when present, reads upto k elements into the array slice
        slice, where k is the size of the slice. This function returns
        the number of elements actually read, which will be less than
        or equal to k. If no elements remain before the end-of-stream,
        it returns 0 (this function also returns 0 when slice is
        empty). It blocks, if necessary, until at least one element is
        available.

    readVecNB(n)

        when present, reads i elements without blocking for 1 <= i <=
        n, creating a vector v, returning SOME(v); or if end-of-stream
        is detected without blocking, returns SOME(fromList[]); or if
        a read would block, returns NONE.

    readArrNB(slice)

        when present, reads, without blocking, upto k elements into
        the array slice slice, where k is the size of the slice. If
        this function would block (i.e. no elements are available and
        the end-of-stream has no been detected), then it returns NONE,
        otherwise it returns SOME(n), where n is the number of
        elements actually read (0 on end-of-stream)

    block()

        when present, blocks until at least one element is available
        for reading without blocking, or until an end-of-stream
        condition is detected.

    canInput()

        when present, returns true if and only if the next read can
        proceed without blocking.

    avail()

        returns the number of bytes available on the ``device,'' or
        NONE if it cannot be determined. For files or strings, this is
        the file or string size minus the current position; for most
        other input sources, this is probably NONE. This can be used
        as a hint by inputAll. Note that this is a byte count, not an
        element count.

    getPos()

        when present, returns the current position in the file. The
        getPos function must be non-decreasing (in the absence of
        setPos operations or other interference on the underlying
        object).

    setPos(i)

        when present, moves to position i in file.

    endPos()

        when present, returns the position corresponding to the end of
        the file without actually changing the current position.

    verifyPos()

        when present, returns the true current position in the
        file. It is similar to getPos, except that the latter may
        maintain its own notion of file position for efficiency,
        whereas verifyPos will typically perform a system call to
        obtain the underlying operating system's value of the file
        position.

    close

        marks the reader closed and, if necessary, performs any
        cleanup and releases any operating system resources. Further
        operations on the reader (besides close and getPos) raise
        IO.ClosedStream.

    ioDesc

        when present, is the abstract operating system descriptor
        associated with this stream.

    One of readVec, readVecNB, readArr, or readArrNB must be
    provided. Providing more of the optional functions increases
    functionality and/or efficiency of clients.

        * Absence of all of readVec, readArr, and block means that
          blocking input is not possible.

        * Absence of all of readVecNB, readArrNB, and canInput means
          that non-blocking input is not possible.

        * Absence of readVecNB means that non-blocking input requires
          two system calls (using canInput and readVec).

        * Absence of readArr or readArrNB means that input into an
          array requires extra copying. Note that the ``lazy
          functional stream'' model does not use arrays at all.

        * Absence of setPos prevents random access.

    Having avail return a value helps the client perform very large
    input more efficiently, with one system call and no copying.

    If the reader can provide more than the minimum set of operations
    in a way that is more efficient then the obvious synthesis (see
    augmentReader), then by all means it should do so. Providing more
    than the minimum by just doing the obvious synthesis inside the
    Primitive I/O layer is not recommended because then clients will
    not get the ``hint'' about which are the efficient
    (``recommended'') operations. Clients concerned with efficiency
    will make use of the operations provided natively, and may need to
    choose algorithms depending on which operations are provided;
    clients not concerned with efficiency or requiring certain
    operations can use the reader constructed by augmentReader.

[datatype writer]

A writer is a file (device, etc.) opened for writing. A writer is an
abstraction for a store of items of type elem. Usually, it will
correspond to a file or device opened for writing. It can also
represent input to some algorithm or function, not necessarily
connected to the outside world, which consumes the output to guide its
computations. The resulting store of elements is potentially
unbounded. In the description below, we will usually refer to the
store as a ``file,'' as this is the most common instance.

    name

        is the name associated with this file or device, for use in
        error messages shown to the user.

    chunkSize

        is the recommended (efficient) size of write operations on
        this writer. This is typically set to the block size of the
        operating system's buffers. chunkSize = 1 strongly recommends
        unbuffered writes, but since buffering is handled at a higher
        level, it cannot guarantee it. chunkSize <= 0 is illegal.

    writeVec(slice)

        when present, writes the elements from the vector slice slice
        to the output device and returns the number of elements
        actually written. If necessary, it blocks until the output
        device can accept at least one element.

    writeArr(slice)

        when present, writes the elements from the array slice slice
        and returns the number of elements actually written. If
        necessary, it blocks until the underlying device can accept at
        least one element.

    writeVecNB(slice)

        when present, attempts to write the elements from the vector
        slice slice to the output device without blocking. If
        successful, it returns SOME(n), where n is the number of
        elements actually written. Otherwise, if it would block then
        it returns NONE without blocking.

    writeVecNB(slice)

        when present, attempts to write the elements from the array
        slice slice to the output device without blocking. If
        successful, it returns SOME(n), where n is the number of
        elements actually written. Otherwise, if it would block then
        it returns NONE without blocking.

    block()

        when present, blocks until the writer is guaranteed to be able
        to write without blocking.

    canOutput()

        when present, returns true if and only if the next write can
        proceed without blocking.

    getPos()

        when present, returns the current position within the file.

    endPos()

        when present, returns the position corresponding to the end of
        the file, without actually changing the current position.

    setPos(i)

        when present, moves to position i in the file, so future
        writes occur at this position.

    verifyPos()

        when present, returns the true current position in the
        file. This is similar to getPos, except that the latter may
        maintain its own notion of file position for efficiency,
        whereas verifyPos will typically perform a system call to
        obtain the underlying operating system's value of the file
        position.

    close()

        marks the writer closed and, if necessary, performs any
        cleanup and releases any operating system resources. Further
        operations (other than close) raise IO.ClosedStream.

    ioDesc

        when present, is the abstract operating system descriptor
        associated with this stream.

    The write operations return the number of full elements that have
    been written. If the size of an element is greater than 1 byte, it
    is possible that an additional part of an element might be
    written. For example, if one tries to write 2 elements, each of
    size 3 bytes, the underlying system write operation may report
    that only 4 of the 6 bytes has been written. Thus, one full
    element has been written, plus part of the second, so the write
    operation would return 1.

    One of writeVec, writeVecNB, writeArr, or writeArrNB must be
    provided. Providing more of the optional functions increases
    functionality and/or efficiency of clients.

        * Absence of all of writeVec, writeArr, and block means that
          blocking output is not possible.

        * Absence of all of writeVecNB, writeArrNB, and canOutput
          means that non-blocking output is not possible.

        * Absence of writeArr or writeArrNB means that extra copying
          will be required to write from an array.

        * Absence of setPos prevents random access.

[openVector v] creates a reader whose content is v.

[nullRd()]
[nullWr()]

These functions create readers and writers for a null device
abstraction. The reader nullRd acts like a reader that is always at
end-of-stream. The writer nullWr serves as a sink; any data written
using it is thrown away. Null readers and writers can be closed; if
closed, they are expected to behave the same as any other closed
reader or writer.

[augmentReader rd] produces a reader in which as many as possible of
readVec, readArr, readVecNB, and readArrNB are provided, by
synthesizing these from the operations of rd.

For example, augmentReader can synthesize readVec from readVecNB and
block, synthesize vector reads from array reads, synthesize array
reads from vector reads, as needed. The following table indicates how
each synthesis can be accomplished.

    Synthesize: 	From:
    readVec             readVec or readArr or (block and (readVecNB
                          or readArrNB))
    readArr             readArr or readVec or (block and (readArrNB
                          or readVecNB))
    readVecNB           readVecNB or readArrNB or (canInput and
                          (readVec or readArr))
    readArrNB           readArrNB or readVecNB or (canInput and
                          (readArr or readVec))

In each case, the synthesized operation may not be as efficient as a
more direct implementation --- for example, it is faster to read data
directly into an array than it is to read it into a vector and then
copy it into the array. But augmentReader should do no harm: if a
reader rd supplies some operation (such as readArr), then
augmentReader(rd) provides the same implementation of that operation,
not a synthesized one.

[augmentWriter wr] produces a writer in which as many as possible of
writeVec, writeArr, writeVecNB, and writeArrNB are provided, by
synthesizing these from the operations of wr.

The following table indicates how each synthesis can be accomplished.

    Synthesize: 	From:
    writeVec            writeVec or writeArr or (block and (writeVecNB
                          or writeArrNB))
    writeArr            writeArr or writeVec or (block and (writeArrNB
                          or writeVecNB))
    writeVecNB          writeVecNB or writeArrNB or (canOutput and
                          (writeVec or writeArr))
    writeArrNB          writeArrNB or writeVecNB or (canOutput and
                          (writeArr or writeVec))

The synthesized operation may not be as efficient as a more direct
implementation, but if a writer supplies some operation, then the
augmented writer provides the same implementation of that operation.

[Discussion]

It may not be possible to use augmentReader or augmentWriter to
synthesize operations in a way that is thread-safe in concurrent
systems.

None of the function components in readers and writers should block,
except for the obvious ones: readVec, readArray, writeVec, writeArray,
and block.

The end-of-stream condition at the Stream I/O level is an artifact of
a read operation at the Primitive I/O level returning 0
elements. Although this event is maintained in the stream, it is a
transient condition at the Primitive I/O level. If a call to readVec
returns an empty vector one time, it is quite possible that another
call to readVec at the same file position will return elements. The
value returned by an endPos function should indicate the position
after the last element in the file at the time the function is called.

    Implementation note:

    The functions getPos, setPos, endPos, and verifyPos are used to
    support arbitrary random access on the underlying I/O file or
    device. On most systems, this is well-supported only on static
    objects such as strings or regular files. Typical implementations
    will therefore set getPos and the rest to NONE in both readers and
    writers for all other file types.

    In an implementation where an input stream is represented as a
    chain of buffers, each buffer may contain, along with its vector
    of data, a pos value indicating where the data came from in the
    underlying file. As each buffer corresponds to a read operation,
    the client is likely to call getPos on each read operation. Thus,
    the reader should, if possible, maintain its own version of the
    file position, to be returned by getPos, to avoid extra system
    calls.

    Unlike readers, which can expect their getPos functions to be
    called frequently, writers need not implement getPos in a highly
    efficient manner: a system call for each getPos is
    acceptable. Indeed, with a file opened for atomic append, the
    information cannot be obtained reliably except by a system call
    using verifyPos.
*)
