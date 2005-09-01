functor B() =
    struct
	structure A = A()
	val d = (A.c,"Today")
    end
