(* Environment with a persistent part to be used accross program units
 * ('a map) and a combined persistent/non-persistent (hashing) map to
 * be used locally for each program unit ('a qmap).  
 *)

signature QUASI_ENV =
  sig

    structure Env : MONO_FINMAP

    type 'a qmap                   (* combined map *)
    type 'a map = 'a Env.map       (* persistent map *)

    type dom = Env.dom
 
    val mk : int -> 'a map -> 'a qmap
    val lookup : 'a qmap -> dom -> 'a option
    val update : dom * 'a * 'a qmap -> unit

    val Fold : (((dom * 'b) * 'a) -> 'a) -> 'a -> 'b qmap -> 'a

    val combine: 'a map * 'a qmap -> 'a qmap

    type StringTree = Env.StringTree
    val layout : {start:string,finish:string,eq:string,sep:string} -> 
      (dom -> StringTree) -> ('a -> StringTree) -> 'a qmap -> StringTree
  end
      