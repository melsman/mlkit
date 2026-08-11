(* Region.sml *)

structure Region : REGION =
  struct
	fun resetRegion `[r] () = prim `[r] ("resetRegion", ())


	fun isAtbot `[r] () = prim `[r] ("is_Atbot", ())

	fun numPagesOfRegion `[r] () = prim `[r] ("num_Pages", ())

	fun memoryUsageOfRegion `[r] () = prim `[r] ("get_Region_Memory_Usage_Bytes", ())


	fun getPageSizeBytes () = prim ("get_Page_Size_Bytes", ())

	fun getNumAllocatedPages () = prim ("get_Num_Allocated_Pages", ())

	fun getFreeListSize () = prim ("get_Free_List_Size", ())


	fun getThreadFreeListSize () = prim ("get_Thread_Free_List_Size", ())

	fun giveThreadFreeListToGlobal () = prim ("give_Thread_Free_List_To_Global", ())
  end
