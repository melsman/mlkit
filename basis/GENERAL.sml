(*GENERAL.sml*)

signature GENERAL = sig
  eqtype unit
  type exn
  eqtype 'a ref

  exception Bind
  exception Match
  exception Subscript
  exception Size
  exception Overflow
  exception Domain
  exception Div
  exception Chr
  exception Fail of string 
  exception Interrupt

  val exnName : exn -> string 
  val exnMessage : exn -> string 

(* now in OPTION signature: (mads 13 March 1998) 
   well, the specs should still be here (Martin-08/07/1998) 
*)

  datatype 'a option = NONE | SOME of 'a 
  exception Option
  val getOpt : ('a option * 'a) -> 'a 
  val isSome : 'a option -> bool 
  val valOf : 'a option -> 'a 

  datatype order = LESS | EQUAL | GREATER
  val ! : 'a ref -> 'a 
  val := : ('a ref * 'a) -> unit 
  val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c 
  val before : ('a * unit) -> 'a 
  val ignore : 'a -> unit 

end; (*signature GENERAL*)

(*
eqtype unit 
     The type containing a single value denoted (), which is typically used
     as a trivial argument or as a return value for a side-effecting
     function. 

type exn 
     The type of values transmitted when an exception is raised and
     handled. This type is special in that it behaves like a datatype with an
     extensible set of data constructors, where new constructors are
     created by exception declarations. 

eqtype 'a ref 

exception Bind 
     indicates that pattern matching failed in a val binding. 

exception Match 
     indicates that pattern matching failed in a case expression or function
     application. 

exception Subscript 
     indicates that an index is out of range, typically arising when the
     program is accessing an element in an aggregate data structure (such
     as a list, string, array or vector). 

exception Size 
     indicates an attempt to create an aggregate data structure (such as an
     array, string or vector) whose size is too large or negative. 

exception Overflow 
     indicates that the result of an arithmetic function is not representable,
     e.g., is too large. (Replaces the Abs, Exp, Mod, Neg, Prod, Quot, and Sum
     exceptions required by the Definition). 

exception Domain 
     indicates that the argument of a mathematical function is outside the
     domain of the function. Raised by functions in structures matching
     the MATH or INT_INF signatures. (Replaces the Sqrt and Ln
     exceptions required by the Definition). 

exception Div 
     indicates an attempt to divide by zero. 

exception Chr 
     indicates an attempt to create a character with a code outside the
     range supported by the underlying character type. 

exception Fail 
     A general-purpose exception to signify the failure of an operation.
     Not raised by any function built into the SML Standard Library. 

exnName ex 
     returns a name for the exception ex. The name returned may be that
     of any exception constructor aliasing with ex. For instance, let
     exception E1; exception E2 = E1 in exnName E2 end might
     evaluate to "E1" or "E2". 

exnMessage ex 
     returns a message corresponding to exception ex. The precise format
     of the message may vary between implementations and locales, but
     will at least contain the string exnName ex. 

         Example:

         exnMessage Div = "Div"
         exnMessage (OS.SysErr ("No such file or directory", NONE)) = 
           "OS.SysErr \"No such file or directory\""



datatype 'a option 
     The type option provides a distinction between some value and no
     value, and is often used for representing the result of partially defined
     functions. It can be viewed as a typed version of the C convention of
     returning a NULL pointer to indicate no value. 

exception Option 

getOpt (opt, a) 
     returns v if opt is SOME v; otherwise returns a. 

isSome opt 
     returns true if opt is SOME v; otherwise returns false. 

valOf opt 
     returns v if opt is SOME v; otherwise raises Option. 

datatype order 
     Values of type order are used when comparing elements of a type that
     has a linear ordering. 

! re 
     returns the value referred to by the reference re. 

re := a 
     makes the reference re refer to the value a. 

f o g 
     is the function composition of f and g. Thus, (f o g) a is equivalent
     to f(g a). 

a before b 
     returns a. It provides a notational shorthand for evaluating a, then b,
     before returning the value of a. 

ignore a 
     returns (). The purpose of ignore is to discard the result of a
     computation, returning () instead. This is useful, for example, when a
     higher-order function, such as List.app, requires a function returning
     unit, but the function to be used returns values of some other type. 



Discussion

Some systems may provide a compatibility mode in which the replaced
exceptions (e.g., Abs, Sqrt) are provided at top-level as aliases for the new
exceptions.
*)