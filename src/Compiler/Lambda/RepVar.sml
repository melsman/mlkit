structure RepVar :> REP_VAR =
struct
  datatype description = Top | Layout of {id: int, span: int}
  type repvar = description URef.uref
  exception Span of int * int
  val counter = ref 0
  val top = URef.uref Top
  fun fresh n =
      if n < 0 then raise Size
      else
        let val id = !counter
            val () = counter := id + 1
        in URef.uref (Layout {id = id, span = n})
        end
  fun isTop r = case URef.!! r of Top => true | _ => false
  fun span r = case URef.!! r of Top => NONE | Layout {span, ...} => SOME span
  fun checkSpan (r, n) =
      case span r of
          NONE => if n < 0 then raise Size else ()
        | SOME m => if m = n then () else raise Span (m, n)
  fun unify (r, s) =
      (* Check before union: a failed unification must leave both classes intact. *)
      let val () = case (span r, span s) of
                       (SOME m, SOME n) => if m = n then () else raise Span (m, n)
                     | _ => ()
      in URef.unify (fn (Top, _) => Top
                     | (_, Top) => Top
                     | (a, _) => a) (r, s)
      end
  fun same (r, s) = URef.eq (r, s) orelse (isTop r andalso isTop s)
  fun layout r =
      case URef.!! r of
          Top => "top"
        | Layout {id, span} => "r" ^ Int.toString id ^ ":" ^ Int.toString span
  fun copier () =
      let val copies = ref []
      in fn r =>
            if isTop r then top
            else case List.find (fn (s, _) => same (r, s)) (!copies) of
                     SOME (_, t) => t
                   | NONE =>
                     let val t = fresh (valOf (span r))
                     in copies := (r, t) :: !copies; t
                     end
      end
  (* Canonical class IDs preserve aliases in one pickle. Unpickling allocates
   * fresh IDs, so independent compilation units never share analysis state. *)
  fun encode r =
      case URef.!! r of Top => NONE | Layout {id, span} => SOME (id, span)
  fun decode NONE = top
    | decode (SOME (_, n)) = fresh n
  val pu = Pickle.convert (decode, encode)
                          (Pickle.optionGen (Pickle.pairGen (Pickle.int, Pickle.int)))
end
