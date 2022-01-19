structure EffVarEnv=
    OrderFinMap(struct
                    type t = Effect.effect
                    val lt = Effect.lt_eps_or_rho
		end)
