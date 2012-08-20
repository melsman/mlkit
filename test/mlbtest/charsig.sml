signature X =
    sig datatype s = S of t
    end

structure K :> X =
    struct
	datatype s = S of t
    end

val f = fn t => K.S t