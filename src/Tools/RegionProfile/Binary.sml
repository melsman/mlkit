(* Version 5: explicit little-endian integers, never native C struct layouts. *)
structure ProfileBinary =
struct
  open ProfileJson
  val magic = "MLKRP\000\005\000"
  fun schema version tag =
      case tag of
          1 => ("header",["word_bytes","page_bytes","gc_enabled"],["main_source"])
        | 2 => ("thread_start",["thread","time"],[])
        | 3 => ("thread_end",["thread","time"],["reason"])
        | 4 => ("session_end",["time","samples","max_pages","gc_collections"],[])
        | 5 => ("binding",["definition","binding"],["unit","name","source","kind","region_type"])
        | 6 => ("sample_begin",["sample","time","requested_time","wait_ns"],["gc_kind","reason"])
        | 7 => ("region",["sample","thread","worker","cpu","definition","g0_pages","g0_unused_tail",
                          "g1_pages","g1_unused_tail","pages","unused_tail","page_footprint",
                          "large_bytes","finite_bytes","descriptor_bytes"],[])
        | 8 => ("stack",["sample","thread","worker","cpu","active_bytes","finite_bytes","stack_bytes"],[])
        | 9 => ("sample_end",["sample","time","frames","pages_visited","cache_pages","cache_bytes","max_pages","gc_collections"],[])
        | 10 => ("sample_skipped",["time"],["reason"])
        | 11 => ("mark",["time"],["label"])
        | 12 => ("allocation_session",["enabled","depth"],["build_id","selector"])
        | 13 => if version = "7" then
            ("allocation_site",["definition","site","point","location_kind"],["unit","function","source","ir_identity","ir_object"])
            else ("allocation_site",["definition","site"],["unit","function","source"])
        | 14 => ("allocation",["thread","definition","count","bytes"],[])
        | 15 => ("allocation_incomplete",["thread"],["reason"])
        | 16 => ("allocation_region",["binding"],["unit","name","source"])
        | _ => raise Fail "unknown binary record tag"
  fun read path =
      let val input = BinIO.openIn path
          val data = (BinIO.inputAll input handle e => (BinIO.closeIn input; raise e))
          val () = BinIO.closeIn input
          val size = Word8Vector.length data
          fun byte p = Word8.toInt(Word8Vector.sub(data,p))
          fun number p n =
              let fun loop i acc = if i < 0 then acc
                                   else loop (i-1) (acc*256 + IntInf.fromInt(byte(p+i)))
              in loop (n-1) 0
              end
          val () = if size >= 8 andalso
                     Byte.bytesToString(Word8VectorSlice.vector(Word8VectorSlice.slice(data,0,SOME 6))) = "MLKRP\000"
                     andalso (byte 6 = 5 orelse byte 6 = 6 orelse byte 6 = 7) andalso byte 7 = 0
                   then () else raise Fail "unsupported binary profile header (expected version 5, 6 or 7)"
          val version = Int.toString(byte 6)
          fun record start stop =
              let val (kind,nums,strs) = schema version (byte start)
                  val pos = ref (start+1)
                  fun take n =
                      if n > stop - !pos then raise Fail "short binary record"
                      else let val p = !pos in pos := p+n; p end
                  fun numeric key =
                      let val n = number (take 8) 8
                          val value =
                              if key = "gc_enabled" then
                                if n = 0 then Bool false else if n = 1 then Bool true
                                else raise Fail "invalid GC enabled flag"
                              else if key = "worker" orelse key = "cpu" then
                                if n = 18446744073709551615 then Num "-1"
                                else if n <= 2147483647 then Num(IntInf.toString n)
                                else raise Fail "invalid worker/CPU identity"
                              else Num(IntInf.toString n)
                      in (key,value)
                      end
                  fun text key =
                      let val n = number (take 4) 4
                          val () = if n <= IntInf.fromInt(stop - !pos) then () else raise Fail "short binary string"
                          val count = IntInf.toInt n
                          val p = take count
                      in (key,Str(Byte.bytesToString(Word8VectorSlice.vector(Word8VectorSlice.slice(data,p,SOME count)))))
                      end
                  val fields = map numeric nums @ map text strs
                  val () = if !pos = stop then () else raise Fail "extra bytes in binary record"
                  val header = if kind = "header" then
                      [("format",Str "mlkit-region-profile"),("version",Num version),("time_unit",Str "ns"),("size_unit",Str "bytes")] else []
              in Obj(("type",Str kind)::header @ fields)
              end
          fun loop pos acc =
              if size-pos < 4 then rev acc
              else let val n = number pos 4
                   in if n = 0 then raise Fail "empty binary record"
                      else if n > IntInf.fromInt(size-pos-4) then rev acc
                      else let val stop = pos+4+IntInf.toInt n
                               val r = record (pos+4) stop
                           in loop stop (r::acc)
                           end
                   end
      in loop 8 []
      end
end
