(** Operations and information about regions. *)

signature REGION =
  sig
	(* \/ r. r * unit -> unit *)
	val resetRegion : unit -> unit
	

	(* \/ r. r * unit -> bool *)
    val isAtbot : unit -> bool

	(* \/ r. r * unit -> int *)
	val numPagesOfRegion : unit -> int

	(* \/ r. r * unit -> int *)
    val memoryUsageOfRegion : unit -> int


	val getPageSizeBytes : unit -> int

	val getNumAllocatedPages : unit -> int
	
	val getFreeListSize: unit -> int


	val getThreadFreeListSize : unit -> int

	val giveThreadFreeListToGlobal : unit -> unit
  end

(**
[resetRegion [r] ()] resets the explicit region parameter. Warnings will be
generated if there are any live references into the region, but the region will
still be reset. This operation it thus not guaranteed to be sound.


[isAtbot [r] ()] returns true if the explicit region parameter is at bottom.

[memoryUsage [r] ()] returns an integer describing the memory usage (in bytes)
of the explicit region parameter.

[numPages [r] ()] returns the number of pages allocated to the explicit region


[getPageSizeBytes ()] returns the size of a memory page (in bytes).

[getNumAllocatedPages ()] returns the total number of allocated pages, including
free list.

[getFreeListSize ()] returns the size of the global free list (in number of pages).


[getThreadFreeListSize ()] returns the size of the thread-local free list (in number of pages). If the program is single-threaded, this is equivalent to getFreeListSize ().

[giveThreadFreeListToGlobal] gives pages of thread local free list back to global free list.
*)
