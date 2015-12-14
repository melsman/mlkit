(* Copyright 2015, Martin Elsman, MIT-license *)

signature FORMLETS = sig
  type key = string
  type label = string
  type value = string

  (* Elements - elements hold strings *)
  type el
  val fromEditCon : value Dojo.editCon -> el
  val textbox     : unit -> el
  val boolbox     : unit -> el
  val intbox      : unit -> el
  val realbox     : unit -> el
  val datebox     : unit -> el
  val selectbox   : (value*label)list -> el
  val hidden      : unit -> el

  val withKey     : el * key -> el
  val withLabel   : el * label -> el
  val withValue   : el * value -> el

  type button
  val button      : label -> button

  (* Forms *)
  type form
  val %           : el -> form
  val %%          : button -> form
  val >>          : form * form -> form
  val />          : form * form -> form
  val group       : label -> form -> form
  val elem        : label option -> Js.elem -> form
  val empty       : form
  val space       : form
  val changer     : el -> (value * form) list -> form
  val hextend     : form -> form

  (* Fields *)
  type 'a f
  val value       : el -> value f
  val readonly    : el -> bool f
  val enabled     : el -> bool f
  val ||          : 'a f * 'b f -> ('a * 'b) f
  val emp         : unit f

  (* Rules *)
  type rule
  val init_rule       : 'a f -> (unit -> 'a) -> rule
  val load_rule       : (unit -> (key*value)list) -> rule
  val update_rule     : 'a f -> 'b f -> ('a -> 'b) -> rule
  val postupdate_rule : 'a f -> 'b f -> ('a -> 'b) -> rule   (* enabled only after data is loaded *)
  val button_rule     : button -> 'a f -> 'b f -> ('a -> 'b) -> rule
  val validate_rule   : 'a f -> ('a -> string option) -> rule
  val submit_rule     : button -> ((key*value option)list -> unit) -> rule
 
  type formlet = form * rule list
  type error_reporter = string -> unit
  val install : Js.elem -> formlet -> error_reporter -> unit
  val mk : formlet -> error_reporter -> ((unit -> unit) * Dojo.widget) Dojo.M
end

(** formlets

  [install e fl err] returns a computation that when executed, will attach
  the formlet fl as a child to the element e.

  Possible early dynamic checks:
   1. no cycles in update rule graph
   2. no cycles in button rules
   3. all rule field ids are present in the form
   4. no element key is overwritten (done)
   5. an element key is defined at most once
   6. elements (their ids) are used at most once in a form

*)
