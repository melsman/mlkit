structure FV = FormVar

val (filename,errs) = FV.getStringErr("clientfile","Filename",FV.emptyErr)
val _ = FV.anyErrors errs

val _ = Web.returnFile (Web.Info.pageRoot() ^ "/apache/upload/files/" ^ filename)
