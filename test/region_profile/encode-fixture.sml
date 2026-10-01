(* Test-only encoder: keep editable JSON fixtures out of rpview's input API. *)
structure Fixture =
struct
  open ProfileJson
  fun encode input output =
      let val stream = TextIO.openIn input
          val text = TextIO.inputAll stream
          val () = TextIO.closeIn stream
          val lines = String.fields (fn c => c = #"\n") text
          fun complete [] = []
            | complete [_] = []
            | complete (s::ss) = parse s :: complete ss
          val records = complete lines
          val _ = ProfileReader.fromRecords records
          fun little n count =
              if count = 0 then []
              else Char.chr(IntInf.toInt(n mod 256)) :: little (n div 256) (count-1)
          fun scalar r key =
              case find r key of
                  SOME (Bool b) => if b then 1 else 0
                | SOME (Num "-1") => 18446744073709551615
                | SOME v => integer v
                | NONE => if List.exists (fn k => k = key) ["time","word_bytes","requested_time","wait_ns","cache_pages","samples"]
                          then if key = "word_bytes" then 8 else 0
                          else raise Fail ("missing " ^ key)
          fun record r =
              let fun tag n = if n > 11 then raise Fail "unknown fixture record"
                              else if #1(ProfileBinary.schema n) = kind r then n else tag(n+1)
                  val t = tag 1
                  val (_,nums,strs) = ProfileBinary.schema t
                  fun bytes key =
                      let val s = case find r key of SOME v => string v | NONE => ""
                      in implode(little (IntInf.fromInt(size s)) 4) ^ s
                      end
                  val payload = str(Char.chr t) ^ String.concat(map (fn k => implode(little (scalar r k) 8)) nums) ^ String.concat(map bytes strs)
              in implode(little (IntInf.fromInt(size payload)) 4) ^ payload
              end
          val data = ProfileBinary.magic ^ String.concat(map record records)
          val out = BinIO.openOut output
      in BinIO.output(out,Byte.stringToBytes data); BinIO.closeOut out
      end
end
val () = (case CommandLine.arguments() of [input,output] => Fixture.encode input output
                                      | _ => raise Fail "expected input and output")
         handle e => (TextIO.output(TextIO.stdErr,(case e of Fail s => s | _ => General.exnMessage e) ^ "\n"); OS.Process.exit OS.Process.failure)
