(*$SPREAD_DATATYPE: LAMBDA_EXP REGION_EXP*)
signature SPREAD_DATATYPE = 
sig
  type rse
  structure LambdaExp: LAMBDA_EXP
  type cone
  structure RegionExp: REGION_EXP
  val spreadDatbinds: rse -> LambdaExp.datbinds -> cone -> rse * RegionExp.datbinds
end;