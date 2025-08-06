structure InstsBase :> INSTS_BASE = struct

  structure Labels = AddressLabels
  type label = Labels.label

  datatype lab =
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        jumps to millicode, code label, finish
			        label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

  fun eq_lab (DatLab label1, DatLab label2) = Labels.eq(label1,label2)
    | eq_lab (LocalLab label1, LocalLab label2) = Labels.eq(label1,label2)
    | eq_lab (NameLab s1, NameLab s2) = s1 = s2
    | eq_lab (MLFunLab label1, MLFunLab label2) = Labels.eq(label1,label2)
    | eq_lab _ = false

  (* Convert ~n to -n; works for all int32 values including Int32.minInt *)
  fun intToStr (i : IntInf.int) : string =
      let fun tr s = case explode s
                       of #"~"::rest => implode (#"-"::rest)
                        | _ => s
      in tr (IntInf.toString i)
      end

  fun wordToStr (w : IntInf.int) : string =
      "0x" ^ IntInf.fmt StringCvt.HEX w

  (* Convert ~n to -n *)
  fun i2s i = if i >= 0 then Int.toString i
              else "-" ^ Int.toString (~i)

  datatype Offset = WORDS of int
                  | BYTES of int

  fun isZeroOffset (WORDS 0) = true
    | isZeroOffset (BYTES 0) = true
    | isZeroOffset _ = false

  fun offset_bytes (WORDS w) = i2s (8*w)  (* a WORD can contain a ptr or an unboxed integer or word value *)
    | offset_bytes (BYTES b) = i2s b

  structure LabSet : sig type t
                         val insert : t -> lab -> unit
                         val empty  : unit -> t
                         val mem    : t -> lab -> bool
                     end =
    struct
      structure H = Polyhash

      fun hash_s (s:string) : int =
          let val w = Pickle.Hash.hash(Pickle.Hash.string s Pickle.Hash.init)
          in Word.toIntX w
          end

      type t = (lab,unit) H.hash_table

      exception NotThere
      fun key (l:label) = #1(Labels.key l)
      fun hash_lab (l:lab) : int =
          case l of
              LocalLab l => key l
            | DatLab l => key l
            | MLFunLab l => key l
            | NameLab n => hash_s n

      fun empty () : t = H.mkTable (hash_lab,eq_lab) (23,NotThere)
      fun mem (t:t) (l:lab) : bool =
          case H.peek t l of
              SOME _ => true
            | NONE => false

      fun insert (t:t) (l:lab) : unit =
          H.insert t (l,())
    end

  fun memoize f =
      let val r = ref NONE
      in fn () => case !r of SOME v => v
                           | NONE => let val v = f()
                                     in r:=SOME v; v
                                     end
      end

  val sysname =
      memoize (fn () =>
                  case List.find (fn (f,_) => f = "sysname") (Posix.ProcEnv.uname()) of
                      SOME (_, name) => name
                    | NONE => "unknown"
              )

  val release =
      memoize (fn () =>
                  case List.find (fn (f,_) => f = "release") (Posix.ProcEnv.uname()) of
                      SOME (_, v) => SOME v
                    | NONE => NONE
              )

  fun remove_ctrl s =
      CharVector.map (fn c =>
                         if Char.isAlphaNum c orelse c = #"_" orelse c = #"."
                         then c
                         else #"_") s

  fun isDarwin () = sysname() = "Darwin"

  fun pr_namelab s =
      if isDarwin() then "_" ^ s
      else s

  fun pr_lab (DatLab l) = pr_namelab("D." ^ remove_ctrl(Labels.pr_label l))
    | pr_lab (LocalLab l) = "L_" ^ remove_ctrl(Labels.pr_label l)
    | pr_lab (NameLab s) = pr_namelab(remove_ctrl s)
    | pr_lab (MLFunLab l) = pr_namelab("F." ^ remove_ctrl(Labels.pr_label l))

end
