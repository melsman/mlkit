(* Environment with a persistent part to be used accross program units
 * ('a map) and a combined persistent/non-persistent (hashing) map to
 * be used locally for each program unit ('a qmap).  
 *)

signature QUASI_ENV =
  sig

    structure Env : MONO_FINMAP

    type '_a qmap                              (* combined map *)
    type 'a map sharing type Env.map = map     (* persistent map *)

    type dom sharing type Env.dom = dom
 
    val mk : int -> '_a map -> '_a qmap
    val lookup : '_a qmap -> dom -> '_a option
    val update : dom * '_a * '_a qmap -> unit

    val Fold : (((int * '_b) * 'a) -> 'a) -> 'a -> '_b qmap -> 'a

    val combine: 'a map * 'a qmap -> 'a qmap

    type StringTree sharing type StringTree = Env.StringTree
    val layout : {start:string,finish:string,eq:string,sep:string} -> 
      (int -> StringTree) -> ('_a -> StringTree) -> '_a qmap -> StringTree
  end
      