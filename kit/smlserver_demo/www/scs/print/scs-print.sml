val (submit,errs) = ScsFormVar.getEnumErr ["Print","Dublex Print","Update Source"] 
  ("submit","Print or Update button not pressed",ScsFormVar.emptyErr)
val (source,errs) = ScsFormVar.getStringErr ("source","no source provided",errs)
val (printer,errs) = ScsFormVar.getEnumErr (List.map #2 ScsPrint.allPrinters) ("printer","printer",errs)
val (doc_type,errs) = ScsFormVar.getEnumErr (ScsPrint.ppAllDocTypes()) 
  ("doc_type","document type (e.g., LaTeX)",errs)
val (category,errs) = ScsFormVar.wrapMaybe ScsFormVar.getStringErr ("category","no category provided",errs)
val (note,errs) = ScsFormVar.wrapMaybe ScsFormVar.getStringErr ("note","no note provided",errs)
val (on_what_table,errs) = ScsFormVar.wrapMaybe ScsFormVar.getStringErr 
  ("on_what_table","no On What Table provided",errs)
val (on_what_id,errs) = ScsFormVar.wrapMaybe (ScsFormVar.wrapIntAsString ScsFormVar.getIntErr) 
  ("on_what_id","no On What ID provided",errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  case submit of
    "Print" => ScsPrint.printDoc category note on_what_table on_what_id 
      (ScsPrint.docTypeFromString doc_type) `^source` printer
  | "Update Source" => ScsPage.returnPg "Printing Documents" (`
      Notice, that you have updated the document source.<p>

      ` ^^ (ScsPrint.printForm category note on_what_table on_what_id (ScsPrint.docTypeFromString doc_type) [`^source`]))
  | _ => ScsError.panic `scs-print.sml: submit ^submit not recognized`
