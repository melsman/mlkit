signature GENERAL = 
  sig
    eqtype unit
    type exn = exn

    exception Bind
    exception Match
    exception Chr
    exception Div
    exception Domain
    exception Fail of string
    exception Overflow
    exception Size
    exception Span
    exception Subscript

    val exnName : exn -> string
    val exnMessage : exn -> string

    datatype order = LESS | EQUAL | GREATER
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
    val before : 'a * unit -> 'a
    val ignore : 'a -> unit 
  end

(*
Description

eqtype unit

    The type containing a single value denoted (), which is typically
    used as a trivial argument or as a return value for a
    side-effecting function.

type exn = exn

    The type of values transmitted when an exception is raised and
    handled. This type is special in that it behaves like a datatype
    with an extensible set of data constructors, where new
    constructors are created by exception declarations.

exception Bind
exception Match

    Exceptions indicating that pattern matching failed in a val
    binding or, respectively, in a case expression or function
    application. This occurs when the matched value is not an instance
    of any of the supplied patterns.

exception Chr

    The exception indicating an attempt to create a character with a
    code outside the range supported by the underlying character type
    (see CHAR.chr).

exception Div

    The exception indicating an attempt to divide by zero. It replaces
    the Mod exception required by the SML'90 Definition.

exception Domain

    The exception indicating that the argument of a mathematical
    function is outside the domain of the function. It is raised by
    functions in structures matching the MATH or INT_INF
    signatures. It replaces the Sqrt and Ln exceptions required by the
    SML'90 Definition.

exception Fail of string

    A general-purpose exception used to signify the failure of an
    operation. It is not raised by any function in the SML Basis
    Library, but is provided for use by users and user-defined
    libraries.

exception Overflow

    The exception indicating that the result of an arithmetic function
    is not representable, in particular, is too large. It replaces the
    Abs, Exp, Neg, Prod, Quot, and Sum exceptions required by the
    SML'90 Definition.

exception Size

    The exception indicating an attempt to create an aggregate data
    structure (such as an array, string, or vector) whose size is too
    large or negative.

exception Span

    The exception indicating an attempt to apply SUBSTRING.span to two
    incompatible substrings.

exception Subscript

    The exception indicating that an index is out of range, typically
    arising when the program is accessing an element in an aggregate
    data structure (such as a list, string, array, or vector).

exnName ex

    returns a name for the exception ex. The name returned may be that
    of any exception constructor aliasing with ex. For instance,

       let exception E1; exception E2 = E1 in exnName E2 end

    might evaluate to "E1" or "E2".

exnMessage ex

    returns a message corresponding to exception ex. The precise
    format of the message may vary between implementations and
    locales, but will at least contain the string exnName ex.

        Example:

	  exnMessage Div = "Div"
	  exnMessage (OS.SysErr ("No such file", NONE)) = 
	    "OS.SysErr \"No such file\""

datatype order = LESS | EQUAL | GREATER

    Values of type order are used when comparing elements of a type
    that has a linear ordering.

! re

    returns the value referred to by the reference re.

re := a

    makes the reference re refer to the value a.

f o g

    is the function composition of f and g. Thus, (f o g) a is
    equivalent to f(g a).

a before b

    returns a. It provides a notational shorthand for evaluating a,
    then b, before returning the value of a.

ignore a

    returns (). The purpose of ignore is to discard the result of a
    computation, returning () instead. This is useful, for example,
    when a higher-order function, such as List.app, requires a
    function returning unit, but the function to be used returns
    values of some other type.
*)
