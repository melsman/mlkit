(*$SPREAD_EXPRESSION: LAMBDA_EXP REGION_EXP REGION_STAT_ENV*)

signature SPREAD_EXPRESSION =
  sig 
    structure E : LAMBDA_EXP
    structure E': REGION_EXP
    structure RegionStatEnv: REGION_STAT_ENV
    type cone and place

    (* if (rse',e') = spreadPgm(rse,e,lvars) succeeds, then
       rse' contains bindings for every constructor and lvar declared in e.
       The constructors declared in e are apparent from e, but the set of
       lvars declared by e are passed to spreadPgm as the third argument.

       Moreover, e' is the spread version of e, explicitly annotated with
       fresh region variables and fully quantified region type schemes.
    *)

    val spreadPgm: cone * RegionStatEnv.regionStatEnv * E.LambdaPgm  ->
                   cone * RegionStatEnv.regionStatEnv * (place,unit)E'.LambdaPgm

  end;
