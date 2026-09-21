(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or mlb) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 nooptimiser    ; disable lambda optimiser
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 ue             ; expect uncaught exception

 nogc 			; disable gc
 parallel		; enable parallel computation

Test files may contain Standard ML like comments.

*)

(* -------------------------------------- *)
(* Testing functionality of region module *)
(* -------------------------------------- *)

resetRegion_doesNotIncreaseMemoryUsage.mlb
resetRegion_idempotent_memoryUsage.mlb
resetRegion_keepsAtbot_onEmpty.mlb
resetRegion_setsAtbot_afterAllocation.mlb
resetRegion_resets_with_liveRef.mlb
resetRegion_doesNotIncreaseMemoryUsage.mlb  	 parallel
resetRegion_idempotent_memoryUsage.mlb		 parallel
resetRegion_keepsAtbot_onEmpty.mlb		 parallel
resetRegion_setsAtbot_afterAllocation.mlb	 parallel
resetRegion_resets_with_liveRef.mlb		 parallel

isAtbot_true_initial.mlb
isAtbot_false_afterAllocation.mlb
isAtbot_true_initial.mlb			 parallel
isAtbot_false_afterAllocation.mlb		 parallel

numPages_nonnegative_initial.mlb
numPages_increasing_afterAllocation.mlb
numPages_nonnegative_initial.mlb 		 parallel
numPages_increasing_afterAllocation.mlb 	 parallel

memoryUsage_nonnegative_initial.mlb
memoryUsage_increases_afterAllocation.mlb
memoryUsage_nonnegative_initial.mlb		 parallel
memoryUsage_increases_afterAllocation.mlb   	 parallel

getPageSizeBytes_stable.mlb
getPageSizeBytes_positive.mlb
getPageSizeBytes_stable.mlb			 parallel
getPageSizeBytes_positive.mlb			 parallel

getNumAllocatedPages_nonnegative.mlb
getNumAllocatedPages_getFreeList.mlb
getNumAllocatedPages_nonnegative.mlb		 parallel
getNumAllocatedPages_getFreeList.mlb		 parallel

getFreeListSize_nonnegative.mlb
freeList_lAllocatedPages.mlb
getFreeListSize_nonnegative.mlb			 parallel
freeList_lAllocatedPages.mlb			 parallel

getThreadFreeListSize_nonnegative.mlb
threadFree_lTotalAllocated.mlb
getThreadFreeListSize_nonnegative.mlb		 parallel
threadFree_lTotalAllocated.mlb			 parallel

giveBack_globalFreeList_nondecreasing.mlb
giveBack_idempotent_globalFreeList.mlb
giveBack_idempotent_threadFreeList.mlb
giveBack_preservesFreeListSum.mlb
giveBack_preservesTotalAllocatedPages.mlb
giveBack_threadFreeList_nonincreasing.mlb
giveBack_globalFreeList_nondecreasing.mlb	 parallel
giveBack_idempotent_globalFreeList.mlb		 parallel
giveBack_idempotent_threadFreeList.mlb		 parallel
giveBack_preservesFreeListSum.mlb		 parallel
giveBack_preservesTotalAllocatedPages.mlb	 parallel
giveBack_threadFreeList_nonincreasing.mlb	 parallel

region_usageWithinAllocatedPages.mlb
region_usageWithinAllocatedPages.mlb		 parallel

(* -------------------------------------- *)
(* Testing Size combinator module         *)
(* -------------------------------------- *)

size_int_zero.mlb
size_string_positive.mlb
size_list_nil_is_word.mlb
size_list_grows.mlb
size_tup2_positive.mlb
size_option_some_gt_none.mlb
size_leq_regionUsage.mlb

size_int_zero.mlb				 parallel
size_string_positive.mlb			 parallel
size_list_nil_is_word.mlb			 parallel
size_list_grows.mlb				 parallel
size_tup2_positive.mlb				 parallel
size_option_some_gt_none.mlb		 	 parallel
size_leq_regionUsage.mlb			 parallel
