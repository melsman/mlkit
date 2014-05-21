signature OPTION =
  sig
    datatype 'a option = NONE | SOME of 'a
    exception Option
    val getOpt     : 'a option * 'a -> 'a
    val isSome     : 'a option -> bool
    val valOf      : 'a option -> 'a
    val filter     : ('a -> bool) -> 'a -> 'a option
    val join       : 'a option option -> 'a option
    val app        : ('a -> unit) -> 'a option -> unit
    val map        : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial : ('a -> 'b option)
	             -> 'a option -> 'b option
    val compose    : ('a -> 'b) * ('c -> 'a option)
	             -> 'c -> 'b option
    val composePartial : ('a -> 'b option) * ('c -> 'a option)
	                 -> 'c -> 'b option 
  end

(*
Description

datatype 'a option = NONE | SOME of 'a

    The type option provides a distinction between some value and no
    value, and is often used for representing the result of partially
    defined functions. It can be viewed as a typed version of the C
    convention of returning a NULL pointer to indicate no value.

getOpt (opt, a)

    returns v if opt is SOME(v); otherwise it returns a.

isSome opt

    returns true if opt is SOME(v); otherwise it returns false.

valOf opt

    returns v if opt is SOME(v); otherwise it raises the Option
    exception.

filter f a

    returns SOME(a) if f(a) is true and NONE otherwise.

val join : 'a option option -> 'a option

    The join function maps NONE to NONE and SOME(v) to v.

app f opt

    applies the function f to the value v if opt is SOME(v), and
    otherwise does nothing.

map f opt

    maps NONE to NONE and SOME(v) to SOME(f v).

mapPartial f opt

    maps NONE to NONE and SOME(v) to f(v). The expression mapPartial f
    is equivalent to join o (map f).

compose (f, g) a

    returns NONE if g(a) is NONE; otherwise, if g(a) is SOME(v), it
    returns SOME(f v). Thus, the compose function composes f with the
    partial function g to produce another partial function. The
    expression compose (f, g) is equivalent to (map f) o g.

composePartial (f, g) a

    returns NONE if g(a) is NONE; otherwise, if g(a) is SOME(v), it
    returns f(v). Thus, the composePartial function composes the two
    partial functions f and g to produce another partial function. The
    expression composePartial (f, g) is equivalent to (mapPartial f) o
    g.
*)