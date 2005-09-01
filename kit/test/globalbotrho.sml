fun go () a : ('a -> 'b) * ('a -> unit) = 
    let val b =  ref 7
    in 
	(fn a => raise Match,
	 fn a => ())
    end