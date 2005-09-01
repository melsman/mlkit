structure FV = FormVar

val (filename,errs) = FV.getStringErr("clientfile","Filename",FV.emptyErr)
val (filesize,errs) = FV.getIntErr("clientfile.filesize","Filesize",errs)
val filename_contenttype = FV.wrapOpt FV.getStringErr "clientfile.content-type"
val _ = FV.anyErrors errs

val _ = 
  if filesize > 1024*10 then
    (Page.return "Uploading files in SMLserver" 
     (`The file ^filename of size ^(Int.toString (Int.div (filesize,1024))) Kb 
       is too large. The maximum size is 10Kb.`);
     Web.exit())
  else
    Web.Conn.storeMultiformData("clientfile",Web.Info.pageRoot() ^ "/apache/upload/files/" ^ filename)

val _ = Page.return "Uploading files in SMLserver"
  `Received the following form variables:

  <table border="1">
  <tr><td>Form variable</td><td>Value</td></tr>
  <tr><td>clientfile</td><td>^filename</td></tr>
  <tr><td>filesize</td><td>^(Int.toString filesize) bytes</td></tr>
  ^(case filename_contenttype of
      SOME c => Quot.toString `<tr><td>clientfile.contenttype</td><td>^c</td></tr>`
    | _ => "")
  </table><p>

The file <code>^filename</code> has now been upload.<p>

<a href="upload_form.sml">Back to the index page</a>.`

