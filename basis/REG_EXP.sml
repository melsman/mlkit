(* NFA implementation of regular expression matching by Ken Friis
 * Larsen. Support for regular expression classes and parenthesis
 * extraction by Martin Elsman. mael 2001-09-29. *)

signature REG_EXP =
  sig
    type regexp
    val fromString : string -> regexp 
    val match      : regexp -> string -> bool
    val extract    : regexp -> string -> string list option
  end

(* 
 [regexp] is the type of regular expressions.

 [fromString s] returns a regular expression by parsing the string s
 according to the syntax given for regular expressions below. Raises
 (Fail msg) in case s is not a regular expression according to the
 syntax below.

 [match r s] returns true if the regular expression r matches the
 string s according to the description below. Returns false if the
 regular expression r does not match the string s.

 [extract r s] returns NONE if the regular expression r does not match
 the string s. Returns SOME l if the regular expression r matches the
 string s; the list l is a list of all substrings in s that is matched
 by some regular expression appearing in parentheses in r. Strings in
 l appear in the same order as they appear in s. Nested parentheses
 are supported, but empty substrings of s that are matched by a
 regular expression appearing in a parenthesis in r are not listed in
 l.

 Grammar for regular expressions (RegExp):

    re ::= re1 "|" re2         re1 or re2
        |  re1 re2             re1 followed by re2
        |  re "*"              re repeated zero or more times
        |  re "+"              re repeated one or more times
        |  re "?"              re zero or one time
        |  "(" re ")"          re
        |  c                   specific character
        |  "\" c               escaped character; c is one of |,*,+,?,(,),[,],$,.,\,t,n,v,f,r
        |  "[" class "]"       character class
        |  "[^" class "]"      negated character class
        |  $                   empty string
        |  .                   any character

    class ::=  c               specific character
           |   "\" c           escaped character; c is one of [,],-,\,t,n,v,f,r
           |   c1 "-" c2       ascii character range
           |                   empty class 
           |   class class     composition
            
 Whitespace is significant.  Special characters can be escaped by \  
*)
