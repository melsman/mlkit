val (submit,errs) = ScsFormVar.getEnumErr ["Print","Dublex Print","Update Source"] 
  ("submit","Print or Update button not pressed",ScsFormVar.emptyErr)
val (source,errs) = ScsFormVar.getStringErr ("source","no source provided",errs)
val (printer,errs) = ScsFormVar.getEnumErr ScsPrint.allPrinters ("printer","printer",errs)
val (doc_type,errs) = ScsFormVar.getEnumErr (ScsPrint.ppAllDocTypes()) 
  ("doc_type","document type (e.g., LaTeX)",errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  case submit of
    "Print" => ScsPrint.printDoc (ScsPrint.docTypeFromString doc_type) source printer
  | "Update Source" => ScsPage.returnPg "Printing Documents" (`
      Notice, that you have updated the document source.<p>

      ` ^^ (ScsPrint.printForm (ScsPrint.docTypeFromString doc_type) `^source`))
  | _ => ScsError.panic `scs-print.sml: submit ^submit not recognized`
