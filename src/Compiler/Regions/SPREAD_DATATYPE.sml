(*
*   The purpose of SpreadDatatype is to analyse datatype declarations
*   and find out for each type name what its arity is (not just the
*   type arity, which is given in the input program) but also the
*   region and effect arity). Moreover, the module infers a region-polymorphic
*   type scheme for each value constructor declared in the source program.
*)

signature SPREAD_DATATYPE =
sig
  type rse
  structure LambdaExp: LAMBDA_EXP
  type cone
  structure RegionExp: REGION_EXP
  val spreadDatbinds: rse -> LambdaExp.datbinds -> cone -> rse * RegionExp.datbinds
end
