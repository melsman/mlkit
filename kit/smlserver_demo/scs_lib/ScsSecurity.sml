structure ScsSecurity :> SCS_SECURITY =
  struct
    fun randomChar () = 
      String.sub ("123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz./",
		  Random.range (0,61) (Random.newgen ()))
  end