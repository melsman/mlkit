(* old-string.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories
 *
 * This is an implementation of the old-style string module.
 *)

structure OldString =
  struct

    type string = string

    exception Substring = General.Subscript
    exception Ord = General.Subscript
    exception Chr = General.Chr

    val length = size

    val ord = fn s => case explode s
			of c :: _ => ord c
			 | _ => raise Ord
    val chr = fn i => str(chr i)
 
    val implode = concat
    val explode = (map str) o explode


    val print = TextIO.print

  (* the following are unchanged *)
    val size      = size
    val substring = substring
  end

