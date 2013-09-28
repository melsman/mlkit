(** Reactive Web Programming Library. 

Combinators for constructing reactive web programs. See 
http://www.smlserver.org/smltojs/slides_diku_2007-11-27.pdf for an
introduction to the use of the combinators.
*)

signature RWP =
  sig
    eqtype B eqtype E         (* Kinds: Behaviors (B) and Events (E) *)
    type ('a,'k)t
    type 'a b = ('a, B)t      (* Behaviors *)
    type 'a e = ('a, E)t      (* Events *)

    (* Arrow operations *)
    type ('a,'b,'k)arr = ('a,'k)t -> ('b,'k)t
    val arr : (''b -> ''c) -> (''b,''c,'k) arr
    val >>> : (''b,''c,'k)arr * (''c,''d,'k)arr -> (''b,''d,'k)arr
    val fst : (''b,''c,'k)arr -> (''b*''d,''c*''d,'k)arr
    val snd : (''b,''c,'k)arr -> (''d*''b,''d*''c,'k)arr
    val *** : (''b,''c,'k)arr * (''d,''e,'k)arr -> (''b*''d,''c*''e,'k)arr
    val &&& : (''b,''c,'k)arr * (''b,''d,'k)arr -> (''b,''c*''d,'k)arr

    (* Behavior operations *)
    val timer     : int -> Time.time b
    val textField : string -> string b
    val mouseOver : string -> bool b
    val mouse     : unit -> (int*int) b
    val pair      : ''a b * ''b b -> (''a * ''b) b
    val tup3      : ''a b * ''b b * ''c b -> (''a * ''b * ''c) b
    val list      : ''a b list -> ''a list b
    val delay     : int -> (''a,''a,B)arr
    val calm      : int -> (''a,''a,B)arr
    val const     : ''a -> ''a b
    val iff       : bool b * ''a b * ''a b -> ''a b
    val when      : bool b * ''a b -> ''a b
    val until     : bool b * ''a b -> ''a b
    val current   : ''a b -> ''a
    val poll      : (unit -> ''a) -> int -> ''a b
    val insertDOM : string -> string b -> unit
    val setStyle  : string -> (string * string b) -> unit
    val flatten   : ''a b b -> ''a b

    (* Event operations *)
    val merge     : ''a e * ''a e -> ''a e
    val fold      : (''a * ''b -> ''b) -> ''b -> ''a e -> ''b e
    val click     : string -> ''a -> ''a e
    val empty     : unit -> ''a e

    (* Mixed and General operations *)
    val changes   : ''a b -> ''a e
    val hold      : ''a -> ''a e -> ''a b
    val send      : (''a,'k)t -> ''a -> unit
    val addListener : (''a,'k)t -> (''a -> unit) -> unit

    (* Element operations *)
    val insertDOM_elem : Js.elem -> string b -> unit
    val setStyle_elem  : Js.elem -> (string * string b) -> unit
    val textField_elem : Js.elem -> string b
    val mouseOver_elem : Js.elem -> bool b
    val click_elem     : Js.elem -> ''a -> ''a e

    val mouse_doc      : Js.doc -> (int*int) b
  end

(**

[type 'a b] type of behavior with underlying values of type 'a.

[type 'a e] type of event stream with underlying values of type 'a.

[type ('b,'c,'k)arr] type of behavior (kind 'k = B) or event stream
(kind 'k = E) transformers from type 'b to type 'c.

[arr f] returns a transformer by lifting the function f to work on
behaviors or events.

[f >>> g] returns the transformer resulting from composing f and
g. From the arrow laws, we have "arr f >>> arr g == arr (g o f)".

[fst f] returns a pair-transformer that works as f on the first
component and as the identity on the second component.

[snd f] returns a pair-transformer that works as f on the second
component and as the identity on the first component.

[f *** g] returns a pair-transformer that works as f on the first
component and as g on the second component.

[f &&& g] returns a transformer that given input x will generate
pairs (f x, g x) as outputs.

[timer n] returns a time behavior that updates itself every n
microseconds.

[textField id] returns a string behavior holding the current content
of an input field value identified by id. Raises Fail if there is no
element identified by id in the DOM.

[mouseover id] returns a boolean behavior with a value indicating
whether the mouse is over the element identified by id. Raises Fail
if there is no element identified by id in the DOM.

[mouse()] returns a pair behavior for the x-y positions of the mouse
relative to the upper-left corner of the browser window.

[pair(b1,b2)] returns a behavior for the pair of the two behaviors b1
and b2. Sem[pair(b1,b2)] = \t.(Sem[b1]t,Sem[b2]t).

[merge(e1,e2)] returns the event stream resulting from merging the
two event streams e1 and e2.

[delay n b] returns a behavior equal to b but delayed n microseconds.

[calm n b] returns a behavior equal to b but which is updated only
when there has been no changes in b for n microseconds.

[fold f a e] returns an event stream resulting from accumulating the
results of calling f on events in e.

[click id a] returns an event stream (of a's) representing clicks on
an element identified by id. Raises Fail if there is no element
identified by id in the DOM.

[changes b] returns an event stream representing changes to the
bahavior b.

[hold a es] returns a behavior holding the value of the previous
element in the event stream es with a being the initial value.

[const a] returns the constant behavior with value a. Sem(const a) =
\t.a.

[empty()] returns the empty event stream.

[iff (x,y,z)] returns the behavior which is y when x is true and z
when x is false. Sem[iff (x,y,z)] = \t.if Sem[x](t) then Sem[y](t)
else Sem[z](t).

[when (x,y)] returns the behavior that changes according to y when x
is true and otherwise does not change. The initial value of the
resulting behavior is identical to the current value of y. Sem[when(x,y)] =
\t.if Sem[x](t) then Sem[y](t) else Sem[y](t-delta).

[until (x,y)] returns the behavior that changes according to y until
x becomes true the first time. After this, the resulting behavior is
constant. The initial value of the resulting behavior is identical to
the current value of y.

[current b] returns the current value of the behavior b.

[poll f n] returns a behavior holding the values resulting from
calling the function f every n microseconds.

[insertDOM id b] effectively inserts the behavior in the DOM tree
under the element identified by id. Raises Fail if there is no
element identified by id in the DOM.

*)
