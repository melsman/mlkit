(*$DROP_REGIONS*)

signature DROP_REGIONS =
  sig
        type ('a,'b,'c)LambdaPgm
         and place
         and mul
	 and 'a at
	 and env
	 and lvar

	val empty : env
	val init : env
	val plus : env * env -> env

	val restrict : env * lvar list -> env
	val enrich : env * env -> bool

	val drop_regions : env * (place at, place*mul, unit)LambdaPgm -> 
	                   (place at, place*mul, unit)LambdaPgm * env

	val drop_places : place list -> place list

	type StringTree
	val layout_env : env -> StringTree
  end

