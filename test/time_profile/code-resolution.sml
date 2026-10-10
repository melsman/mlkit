open ProfileJson
fun num n = Num(IntInf.toString n)
fun assert true = ()
  | assert false = raise Fail "PC resolution assertion"
val uuid = "0123456789abcdef0123456789abcdef"
val base = 18014398509481984 : IntInf.int
val session = Obj[("metadata_version",num 1),("function_count",num 2),("scope",Str "static-executable")]
val image = Obj[("image",num 1),("load_address",num base),
                ("build_id",Str uuid),("path",Str "test-image")]
fun function name start finish = Obj[("image",num 1),("start",num start),("end",num finish),
    ("unit",Str "unit"),("function",Str name),("source",Str "source.sml"),("ir_identity",Str "ir")]
fun metadata session images functions = Obj[("code_metadata",session),
    ("code_images",Arr images),("code_functions",Arr functions)]
val first = function "first" 100 200
val second = function "tail-target" 200 220
val resolve = ProfileCode.resolver (metadata session [image] [second,first])
fun check pc expected = assert(string(get (resolve {pc = pc,buildId = SOME uuid}) "status") = expected)
val _ = check (base+99) "unknown-pc"
val _ = check (base+100) "function"
val _ = check (base+199) "function"
val _ = assert(string(get (resolve {pc = base+200,buildId = NONE}) "function") = "tail-target")
val _ = check (base+220) "unknown-pc"
val _ = check (base+100000) "unknown-pc"
val _ = assert(string(get (resolve {pc = base+100,buildId = SOME "other-build"}) "status") = "build-mismatch")
val unavailable = ProfileCode.resolver (metadata Null [] [])
val _ = assert(string(get (unavailable {pc = base+100,buildId = NONE}) "status") = "metadata-unavailable")
val truncated = ProfileCode.resolver (metadata session [image] [first])
val _ = assert(string(get (truncated {pc = base+100,buildId = NONE}) "status") = "metadata-unavailable")
fun rejects m = ((ignore(ProfileCode.resolver m); raise Fail "accepted invalid code metadata")
                 handle Fail "accepted invalid code metadata" => raise Fail "accepted invalid code metadata"
                      | Fail _ => ())
val _ = rejects (metadata session [image] [first,function "overlap" 199 210])
val _ = rejects (metadata session [image] [first,function "first" 300 400])
val _ = rejects (metadata session [image,image] [first])
val _ = rejects (metadata session [] [first])
val _ = rejects (metadata session [image] [function "empty" 100 100])
val _ = rejects (metadata session [image] [function "overflow" 100 ProfileCode.addressLimit])
val _ = rejects (metadata (Obj[("metadata_version",num 2),("function_count",num 1),("scope",Str "static-executable")]) [image] [first])
val _ = rejects (metadata (Obj[("metadata_version",num 1),("function_count",num 1),("scope",Str "unsupported-repl")]) [image] [first])
val _ = rejects (Obj(fields (metadata session [image] [first]) @ [("complete",Bool true)]))
val _ = print "Offline PC resolution boundaries, identities, unknowns, and malformed metadata passed\n"
