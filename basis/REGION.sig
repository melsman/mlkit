(** Operations and information about regions. *)

signature REGION = sig
  val resetRegion                : unit -> unit  (* \/ r. r * unit -> unit *)
  val isAtbot                    : unit -> bool  (* \/ r. r * unit -> bool *)
  val numPagesOfRegion           : unit -> int   (* \/ r. r * unit -> int *)
  val memoryUsageOfRegion        : unit -> int   (* \/ r. r * unit -> int *)
  val getPageSizeBytes           : unit -> int
  val getNumAllocatedPages       : unit -> int
  val getFreeListSize            : unit -> int
  val getThreadFreeListSize      : unit -> int
  val giveThreadFreeListToGlobal : unit -> unit
end

(**

[resetRegion [r] ()] resets the region denoted by the explicit region
parameter. Warnings will be generated if there are any live references into the
region, but the region will still be reset. This operation it thus not
guaranteed to be safe.

[isAtbot [r] ()] returns true if the explicit region parameter has its atbot-bit
set.

[memoryUsage [r] ()] returns an integer describing the memory usage (in bytes)
of the region denoted by the explicit region parameter.

[numPages [r] ()] returns the number of pages allocated to region denoted by the
the explicit region parameter.

[getPageSizeBytes ()] returns the size of a region page (in bytes).

[getNumAllocatedPages ()] returns the total number of allocated region pages,
including free list region pages.

[getFreeListSize ()] returns the size of the global free list (in number of
pages).

[getThreadFreeListSize ()] returns the size of the thread-local free list (in
number of pages). If the program is single-threaded, this is equivalent to
getFreeListSize ().

[giveThreadFreeListToGlobal] gives pages of thread local free list back to the
global free list.

*)
