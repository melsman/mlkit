val (path,errs)   = FormVar.getStringErr("path","path",FormVar.emptyErr)
val _ = FormVar.anyErrors errs

val {isAbs,vol,arcs} = Path.fromString path

val _ =
  if  Path.isAbsolute path orelse List.exists (fn arc => arc = Path.parentArc) arcs then
    Page.return "Return File" `The path <i>^path</i> may not be absolute and
                               may not contain parent arcs (..)<p>
                 You must specify a path relative to the server pageroot.`
  else
    Ns.Conn.returnFile(200,"text/plain",Path.concat (Ns.Info.pageRoot(),path))
