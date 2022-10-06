structure StringFinMap = OrderFinMap(struct type t = string
					    fun lt (s1, s2:string) = s1 < s2
				     end)
