
signature DROP_REGIONS =
  sig
        type ('a,'b,'c)LambdaPgm
         and place
         and mul
	 and 'a at
	 and env
	 and top_env
	 and lvar

	val topify : env -> top_env
	val empty : env
	val init : top_env
	val plus : env * env -> env
	val plus' : top_env * top_env -> top_env

	val restrict : top_env * lvar list -> env
	val enrich : top_env * top_env -> bool

	val drop_regions : env * (place at, place*mul, unit)LambdaPgm -> 
	                   (place at, place*mul, unit)LambdaPgm * env

	val drop_places : place list -> place list

	type StringTree
	val layout_env : env -> StringTree
	val layout_top_env : top_env -> StringTree
  end

