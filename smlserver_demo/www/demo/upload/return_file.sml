structure FV = FormVar

val (filename,errs) = FV.getStringErr("clientfile","Filename",FV.emptyErr)
val _ = FV.anyErrors errs

val _ = Ns.returnFile (Ns.Info.pageRoot() ^ "/demo/upload/files/" ^ filename)