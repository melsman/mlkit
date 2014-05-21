val os_dir = FileSys.openDir (Web.Info.pageRoot() ^ "/web/upload/files/")
fun load_files acc =
  let
    val filename = FileSys.readDir os_dir
  in
    case filename of SOME filename => load_files(filename::acc)
                   | NONE => acc
  end

val uploaded_files =
  List.foldl (fn (filename,acc) => 
	      `<li><a href="^(Html.genUrl "return_file.sml" [("clientfile",filename)])"> ^filename</a> ` ^^ 
	      acc) `` (load_files [])
val _ = FileSys.closeDir os_dir

val _ = Page.return "Uploading files in SMLserver"
  (`
<form enctype=multipart/form-data method=post action=upload.sml>
<table>
<tr><td><input type=file size=140 name=clientfile></td>
<td><input type=submit value="Upload it"></td></tr>
</table>
</form><p>

The following files has been uploaded:<p>

<ul>
` ^^ uploaded_files ^^ `
</ul>
`)

