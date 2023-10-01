(*PRETTYPRINT*)
(*
                                                     7 Sept. 89 Mads Tofte
                                                     (revised 21 Dec 95)

This is a pretty-printer in ML which will print everything which you
can represent as a tree of strings.  The interface is given in
the signature PRETTYPRINT below. To use it, you have to build a
`StringTree' which you can then print using `outputTree'.
Alternatively, you can first turn the StringTree into a `minipage'
by calling `format' and then flatten it into a single formatted string
by calling `flatten'. (I strongly recommend using `outputTree', which
is much more efficient than `formal' and `flatten'; the latter are
only provided for backwards compatability.)


    A StringTree can be one of three things:
      (1) a LEAF, containing a string which will not
          be decomposed or split over lines;
      (2) a NODE with a `start' string,
          a `finish' string, a list of `children' and a
          `childsep', which is the separator that will be used
          between children if there are more than two;
      (3) a HNODE, (H for "horizontal") which is similar to a NODE,
          but has no indent field;

    A StringTree of kind NODE will be printed on one line, if
possible. Otherwise, it will be printed by putting the children
vertically between the `start' string and the `finish' string.  In the
latter case every child is indented by `indent' blanks.  `start' and
`finish' can be empty strings.  Furthermore, when n children are
vertically adjacent and the separator is LEFT, the separator will
appear left-adjusted in front of the children 2 -- n and if the
separator is RIGHT it will appear appended to the end of child 1 --
n-1.

   A StringTree of kind HNODE will be printed as though it had kind
NODE, except that the children of a HNODE each are printed in their
flat version (no line breaks inside any of them), as many as possible on each
line.  This is useful for printing lists of integers or short strings,
for example.  When Flags.raggedRight is turned on, at least one child
will be printed on each line, even if this exceeds the page width.

    In a call `format(hsize: int, t: StringTree)', hsize is the width
in characters of the minipage to be produced. hsize must be >= 3,
so that there is at least space for printing "..." . The meaning of
"..." in the output is : "something goes here, but I don't have the
space to show you what it is".

    A call `outputTree(device,t,width)' prints the tree t within width
`width' calling `device' each time a line is to be output; Thus
`device' is supposed to output the line (without inserting leading or
trailing newlines).

    You are responsible for putting white space in the start, finish,
and childsep strings to ensure that atoms don't get stuck
together. So, for (e.g.) a local declaration, start would be "local ",
finish would be " end", and childsep would (probably, depending on
taste) be (LEFT " in ").  LEAF trees don't need surrounding
spaces. PrettyPrint removes leading spaces from any childsep or finish
string which ends up at the start of a line (such as " in " or " end"
above), to preserve left justification.

*)

signature PRETTYPRINT =
  sig
    val raggedRight : bool ref
    val colwidth : int ref


    datatype StringTree = LEAF of string
                        | NODE of {start : string, finish: string, indent: int,
                                   children: StringTree list,
                                   childsep: childsep}
                        | HNODE of {start : string, finish: string,
                                    children: StringTree list,
                                    childsep: childsep}
    and childsep = NOSEP | LEFT of string | RIGHT of string

    type minipage

    val layoutAtom: ('a -> string) -> ('a -> StringTree)
			(* Given a simple printing routine, return a function
			   to build a leaf. *)
(*
    val layoutSet: ('a -> StringTree) -> 'a EqSet.Set -> StringTree
*)
    val layout_opt : ('a -> StringTree) -> 'a option -> StringTree
    val layout_pair : ('a -> StringTree) -> ('b -> StringTree)
                      -> 'a * 'b -> StringTree
    val layout_list : ('a -> StringTree) -> 'a list -> StringTree
    val layout_set : ('a -> StringTree) -> 'a list -> StringTree
    val layout_together : StringTree list -> int -> StringTree
      (*int is the indent*)

    val flatten1: StringTree -> string
    val oneLiner: ('a -> StringTree) -> ('a -> string)
			(* format a StringTree to a single string of
			   indefinite length. *)

    val format: int * StringTree -> minipage
            (*  ^page width         ^pretty-printed tree *)
            (*  must be >= 3                             *)

    val flatten: minipage -> string
        (* flattens a minipage to a single string putting a newline
           between the lines. *)

    (* outputTree(device,t,width) prints the tree t within width `width'
     calling device each time a line is to be output;
     Thus device is supposed to output the line (without inserting
     leading or trailing newline).
    *)

    val outputTree : (string -> unit) * StringTree * int -> unit

    val printTree : StringTree -> unit

    (* The first argument is a function for pretty-printing n
     * blanks (a multiply of 64) shortly. *)
    val outputTree' : (int -> string) -> (string -> unit) * StringTree * int -> unit

    type Report
    val reportStringTree: StringTree -> Report
    val reportStringTree': int -> StringTree -> Report
      (*                    ^ width *)

  end;
