signature SIG = sig type t val a : t end

structure S : SIG =
    struct
	datatype u = U
	datatype t = T of u
	val a : t = T U
    end