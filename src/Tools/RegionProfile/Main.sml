structure ProfileMain =
struct
  fun run args =
      let val file = ref "profile.rp"
          val haveFile = ref false
          val irRoots = ref []
          val output = ref "profile.html"
          val format = ref ""
          val haveOutput = ref false
          val resolvePC = ref (NONE : IntInf.int option)
          val imageBuild = ref (NONE : string option)
          val settings = ref ([] : (string * ProfileJson.t) list)
          fun setting k v = settings := (k,v)::List.filter (fn (key,_) => key <> k) (!settings)
          fun choice k value choices =
              if List.exists (fn x => x = value) choices then setting k (ProfileJson.Str value)
              else raise Fail ("invalid " ^ k ^ ": " ^ value)
          fun natural s =
              if size s > 0 andalso List.all Char.isDigit (explode s) then Int.fromString s else NONE
          fun address s =
              let val hex = String.isPrefix "0x" s orelse String.isPrefix "0X" s
                  val digits = if hex then String.extract(s,2,NONE) else s
                  val valid = size digits > 0 andalso List.all
                    (if hex then Char.isHexDigit else Char.isDigit) (explode digits)
                  val parsed = if valid then StringCvt.scanString
                    (IntInf.scan (if hex then StringCvt.HEX else StringCvt.DEC)) digits else NONE
              in case parsed of SOME n => if n < ProfileCode.addressLimit then n
                                          else raise Fail "PC exceeds 64 bits"
                              | NONE => raise Fail "PC must be decimal or 0x hexadecimal"
              end
          fun help () =
              (print "Usage: rpview [profile.rp] [-o output.html|output.svg|output.json] [options]\n\
                     \  --format html|svg|json  Infer from output extension; JSON defaults to stdout\n\
                     \  --ir-dir DIR            Fallback search for moved .o.ir files (repeatable)\n\
                     \  --resolve-pc ADDRESS    Resolve a recorded absolute PC; output JSON\n\
                     \  --image-build UUID      Require matching image identity for PC resolution\n\
                     \  --caption TEXT          Override the profile caption\n\
                     \  --sites                 SVG: selected region split by allocation site\n\
                     \  --region rN              SVG: site contributions to region rN\n\
                     \  --regions N             Largest regions/sites to show (default 9; 0 = all)\n\
                     \  --metric NAME           total (default), stack, pages, page_footprint,\n\
                     \                          large_bytes, finite_bytes, descriptor_bytes\n\
                     \  --scope VIEW            all (default), thread:N, worker:N, cpu:N\n\
                     \                          Use -1 for unavailable worker/CPU identity\n\
                     \  --group NAME            HTML table: aggregate, region, thread, worker\n\
                     \  --show-base / --hide-base       Base names (default hidden)\n\
                     \  --show-type / --hide-type       Region type (default hidden)\n\
                     \  --show-peak / --hide-peak       Peak page capacity (default hidden)\n\
                     \  The legend always appears on the right\n";
               OS.Process.exit OS.Process.success)
          fun options [] = ()
            | options ("--sites"::rest) = (setting "sites" (ProfileJson.Bool true); options rest)
            | options ("--region"::value::rest) = (if String.isPrefix "r" value andalso size value > 1 andalso
                  List.all Char.isDigit (explode(String.extract(value,1,NONE))) then
                 (setting "region" (ProfileJson.Str value); setting "sites" (ProfileJson.Bool true); options rest)
               else raise Fail "region must be rN (for example, r163)")
            | options ("--ir-dir"::path::rest) = (irRoots := path :: !irRoots; options rest)
            | options ("--resolve-pc"::value::rest) = (resolvePC := SOME(address value); options rest)
            | options ("--image-build"::value::rest) = (imageBuild := SOME value; options rest)
            | options ("--output"::path::rest) = (output := path; haveOutput := true; options rest)
            | options ("-o"::path::rest) = options ("--output"::path::rest)
            | options ("--format"::value::rest) =
              if value = "html" orelse value = "svg" orelse value = "json" then (format := value; options rest)
              else raise Fail "format must be html, svg or json"
            | options ("--caption"::value::rest) = (setting "caption" (ProfileJson.Str value); options rest)
            | options ("--regions"::value::rest) =
              (case natural value of SOME n => (setting "limit" (ProfileJson.Num(Int.toString n)); options rest)
                                  | NONE => raise Fail "regions must be a nonnegative integer")
            | options ("--metric"::value::rest) =
              (choice "metric" value ["total","stack","pages","page_footprint","large_bytes","finite_bytes","descriptor_bytes"]; options rest)
            | options ("--group"::value::rest) =
              (choice "group" value ["aggregate","region","thread","worker"]; options rest)
            | options ("--scope"::value::rest) =
              let val valid = case String.fields (fn c => c = #":") value of
                                  ["all"] => true
                                | [field,id] => List.exists (fn f => f = field) ["thread","worker","cpu"] andalso
                                    ((size id > 0 andalso List.all Char.isDigit (explode id)) orelse (id = "-1" andalso field <> "thread"))
                                | _ => false
              val canonical = if valid then
                                    (case String.fields (fn c => c = #":") value of
                                         [field,id] => if id = "-1" then value else field ^ ":" ^ IntInf.toString(valOf(IntInf.fromString id))
                                       | _ => value)
                                  else value
              in if valid then (setting "scope" (ProfileJson.Str canonical); options rest)
                 else raise Fail "scope must be all, thread:N, worker:N or cpu:N"
              end
            | options ("--help"::_) = help ()
            | options (value::rest) =
              if List.exists (fn v => value = "--show-" ^ v orelse value = "--hide-" ^ v) ["base","type","peak"] then
                (setting ("show-" ^ String.extract(value,7,NONE)) (ProfileJson.Bool(String.isPrefix "--show-" value)); options rest)
              else if String.isPrefix "-" value orelse !haveFile then raise Fail ("unexpected argument: " ^ value)
              else (file := value; haveFile := true; options rest)
          val () = options args
          val () = if Option.isSome(!imageBuild) andalso not(Option.isSome(!resolvePC))
                   then raise Fail "--image-build requires --resolve-pc" else ()
          val () = if Option.isSome(!resolvePC) andalso not(Option.isSome(!imageBuild))
                   then raise Fail "--resolve-pc requires --image-build from the sampled executable" else ()
          val selected = if Option.isSome(!resolvePC) then "json"
                         else if !format <> "" then !format
                         else if String.isSuffix ".svg" (String.map Char.toLower (!output)) then "svg"
                         else if String.isSuffix ".json" (String.map Char.toLower (!output)) orelse
                                 String.isSuffix ".jsonl" (String.map Char.toLower (!output)) then "json"
                         else if not(!haveOutput) andalso List.exists (fn (k,_) => k = "region") (!settings) then "svg"
                         else "html"
          val sites = List.exists (fn (k,v) => k = "sites" andalso v = ProfileJson.Bool true) (!settings)
          val () = if sites andalso selected <> "svg" then raise Fail "--sites requires SVG output (-o sites.svg or --format svg)" else ()
          val () = if !haveOutput then ()
                   else if selected = "svg" then output := "profile.svg"
                   else if selected = "json" then output := "-" else ()
          val inputId = OS.FileSys.fileId (!file)
          val same = !output <> "-" andalso ((inputId = OS.FileSys.fileId (!output)) handle OS.SysErr _ => false)
          val () = if same then raise Fail "input and output must be different files" else ()
          val records = ProfileBinary.read (!file)
          val profile = ProfileReader.fromRecords records
          val profile = if selected = "html" orelse sites then ProfileIR.enrich (!irRoots) profile else profile
          val config = ProfileJson.Obj (!settings)
          val () = case ProfileJson.find config "scope" of
                       SOME (ProfileJson.Str scope) =>
                         if scope = "all" then () else
                           let val (field,id) = case String.fields (fn c => c = #":") scope of
                                                    [field,id] => (field,id)
                                                  | _ => raise Fail "invalid scope"
                               val rows = List.concat(map (fn s => ProfileSvg.list s "regions" @ ProfileSvg.list s "stacks") (#samples profile))
                               fun matches r = case ProfileJson.find r field of NONE => id = "-1" | _ => ProfileSvg.strField r field = id
                           in if List.exists matches rows then () else raise Fail ("scope not present in profile: " ^ scope)
                           end
                     | _ => ()
          val page = case !resolvePC of
                         SOME pc => ProfileJson.encodeJson
                           (ProfileCode.resolver (#metadata profile) {pc = pc,buildId = !imageBuild}) ^ "\n"
                       | NONE => if selected = "json" then String.concat(map (fn r => ProfileJson.encodeJson r ^ "\n") records)
                     else if selected = "svg" then ProfileSvg.render config profile else ProfilePage.htmlWith config profile
          val out = if !output = "-" then TextIO.stdOut else TextIO.openOut (!output)
          val () = (TextIO.output(out,page) handle e => (TextIO.closeOut out; raise e))
      in if !output = "-" then TextIO.flushOut out else (TextIO.closeOut out; print(!output ^ "\n"))
      end
end
fun profileError message =
    (TextIO.output(TextIO.stdErr,"rpview: " ^ message ^ "\n"); OS.Process.exit OS.Process.failure)
val () = (ProfileMain.run (CommandLine.arguments ())
          handle Fail message => profileError message
               | e => profileError (General.exnMessage e))
