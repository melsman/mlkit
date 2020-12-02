 (* constants that are used in the runtime system and need to be known to the
    compiler in order to issue warnings about array bounds etc.  If you
    change ALLOCATABLE_WORDS_IN_REGION_PAGE or HEADER_WORDS_IN_REGION_PAGE,
    remember also to change src/Runtime/Version17/Region.h.  If you change
    ALLOCATABLE_WORDS_IN_PRIM_ARRAY, remember also to change
    src/Runtime/Version17/Array.h*)

signature REG_CONST = sig
  val MAX_ORIGIN : int
        (*number of distinct region variables in program - needed
  	  for region profiling *)

  val initial_closure_offset : int
        (*initial offset for free variables in a closure*)

  (*sizes in words of storable (boxed) values:*)
  val size_of_real       : unit -> int
  val size_of_ref        : unit -> int
  val size_of_record     : 'a list -> int
  val size_of_blockf64   : 'a list -> int      (* as string representation, which always
                                                * has its size stored *)
  val size_closure       : 'a list * 'b list * 'c list -> int
  val size_fix_closure   : 'a list * 'b list * 'c list -> int
  val size_region_vector : 'a list -> int
  val size_exname        : unit -> int
  val size_excon0        : unit -> int
  val size_excon1        : unit -> int
  val size_con0          : unit -> int
  val size_con1          : unit -> int

  structure TyName : TYNAME

end;
