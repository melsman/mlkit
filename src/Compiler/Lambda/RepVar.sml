structure RepVar :> REP_VAR =
struct
  datatype description = Top | Layout of {id: int, span: int option}
  type repvar = description URef.uref
  exception Span of int * int
  val counter = ref 0
  val top = URef.uref Top
  fun fresh n =
      if n < 0 then raise Size
      else
        let val id = !counter
            val () = counter := id + 1
        in URef.uref (Layout {id = id, span = SOME n})
        end
  fun freshAccess () =
      let val id = !counter
      in counter := id + 1; URef.uref (Layout {id=id,span=NONE})
      end
  fun isTop r = case URef.!! r of Top => true | _ => false
  fun span r = case URef.!! r of Top => NONE | Layout {span, ...} => span
  fun checkSpan (r, n) =
      case URef.!! r of
          Top => if n < 0 then raise Size else ()
        | Layout {span=NONE,...} => raise Span (~1,n)
        | Layout {span=SOME m,...} => if m = n then () else raise Span (m,n)
  fun unify (r, s) =
      (* Check before union: a failed unification must leave both classes intact. *)
      let val () = case (span r, span s) of
                       (SOME m, SOME n) => if m = n then () else raise Span (m, n)
                     | _ => ()
      in URef.unify (fn (Top, _) => Top
                     | (_, Top) => Top
                     | (Layout {id,span=NONE}, Layout {span,...}) => Layout {id=id,span=span}
                     | (a, _) => a) (r, s)
      end
  fun same (r, s) = URef.eq (r, s) orelse (isTop r andalso isTop s)
  fun key r = case URef.!! r of Top => NONE | Layout {id,...} => SOME id
  fun layout r =
      case URef.!! r of
          Top => "top"
        | Layout {id, span} => "r" ^ Int.toString id ^ ":" ^ (case span of SOME n => Int.toString n | NONE => "?")
  fun copier () =
      let val copies = ref []
      in fn r =>
            if isTop r then top
            else case List.find (fn (s, _) => same (r, s)) (!copies) of
                     SOME (_, t) => t
                   | NONE =>
                     let val t = (case span r of SOME n => fresh n | NONE => freshAccess())
                     in copies := (r, t) :: !copies; t
                     end
      end
  (* Canonical class IDs preserve aliases in one pickle. Unpickling allocates
   * fresh IDs, so independent compilation units never share analysis state. *)
  fun encode r =
      case URef.!! r of Top => NONE | Layout {id, span} => SOME (id, span)
  fun decode NONE = top
    | decode (SOME (_, SOME n)) = fresh n
    | decode (SOME (_, NONE)) = freshAccess()
  val pu = Pickle.convert (decode, encode)
                          (Pickle.optionGen (Pickle.pairGen (Pickle.int, Pickle.optionGen Pickle.int)))
end
