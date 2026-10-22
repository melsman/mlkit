fun retained_spawn__noinline (f:unit->unit) : foreignptr =
  prim("spawnone",(prim("pointer",f):foreignptr))
val _:unit = prim("mlkit_spawn_unit_keep",())
