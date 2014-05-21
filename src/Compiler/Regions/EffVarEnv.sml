structure EffVarEnv=
    OrderFinMap(struct
                    type T = Effect.effect
                    fun lt(a: T) b = Effect.lt_eps_or_rho(a,b)
		end)
