signature SCS_PRINT =
  sig
    datatype doc_type = LaTeX
    val docTypeToString      : doc_type -> string
    val docTypeFromString    : string -> doc_type
    val allDocTypes          : doc_type list
    val ppAllDocTypes        : unit -> string list

    val allPrinters   : (string * string) list

    (* Widgets *)
    val choosePrinter : string -> quot * quot
    val printForm     : string -> string -> string -> string -> doc_type -> quot -> quot

    (* Actual Printing *)
    (* bug: must also have an target_url so that you can get from the print-response page back to the origin *)
    val printDoc      : string -> string -> string -> string -> doc_type -> quot -> string -> Ns.status

    val printDocs     : (string * string * string * string * doc_type * quot) list -> string -> Ns.status
  end

structure ScsPrint :> SCS_PRINT =
  struct
    datatype doc_type = LaTeX
    fun docTypeToString LaTeX = "LaTeX"
    fun docTypeFromString doc_type =
      case doc_type of
	"LaTeX" => LaTeX
      | _ => ScsError.panic `ScsPrint.docTypeFromString.doc_type ^doc_type not supported.`
    val allDocTypes = [LaTeX]
    fun ppAllDocTypes() = List.map docTypeToString allDocTypes 

    val allPrinters = List.map (fn p => (p,p)) ["p151","p151d","p152","p152d","p177","p177d","p177t","p233","p233d","p233t"]

    (* Generate file which can be printed and previewed. *)
    (* Files are stored in the scs_print_dir directory.  *)
    local
      val % = ScsDict.d ScsLang.en "scs_lib" "ScsPrint.sml"
      fun getInfo key =
	ScsError.valOf (Ns.Info.configGetValueExact 
			{sectionName="ns/server/"^Ns.Conn.server()^"/SCS",key=key})
(*      fun tmpnam (c: int) : string =
	if c > 10 then ScsError.panic `ScsPrint.tmpnam: Can't create temporary file`
	else
	  let val is = Random.rangelist (97,122) (8, Random.newgen())
	    val f = implode (map Char.chr is)
	  in if FileSys.access(f,[]) then tmpnam(c+1)
	     else f
	  end*)
(*      val scs_print_dir = "scs_print/"*)
      fun path_preview () = Ns.Info.pageRoot() ^ "/" ^ (getInfo "scs_print_preview")
      fun texfile f = path_preview() ^ "/" ^ f ^ ".tex"
      fun psfile f = path_preview() ^ "/" ^ f ^ ".ps"
      fun pdffile f = path_preview() ^ "/" ^ f ^ ".pdf"
      fun dvifile f = path_preview() ^ "/" ^ f ^ ".dvi"
      fun pdfurl f = "/" ^ (getInfo "scs_print_preview") ^ "/" ^ f ^ ".pdf"
    in
      fun genTarget doc_type source =
	case doc_type of
	  LaTeX =>
	    let
	      val tmpfile = ScsFile.uniqueFile (path_preview())
	      val _ = ScsFile.save source (texfile tmpfile)
	      val cmd = Quot.toString `cd ^(path_preview()); latex ^(texfile tmpfile); dvips -o ^(psfile tmpfile) ^(dvifile tmpfile); ps2pdf ^(psfile tmpfile) ^(pdffile tmpfile)`
	    in
	      if Process.system cmd = Process.success
		then pdfurl tmpfile
	      else
		ScsError.panic `ScsPrint.genTarget: Can't execute system command: ^cmd`
	    end
      fun printDoc category note on_what_table on_what_id doc_type source printer =
	case doc_type of
	  LaTeX =>
	    let
	      val print_id = Int.toString (Db.seqNextval "scs_print_id_seq")
	      val batch_id = Int.toString (Db.seqNextval "scs_print_batch_id_seq")
	      val tmpfile = ScsFile.uniqueFile (path_preview()) ^ "-" ^ print_id
	      val _ = ScsFile.save source (texfile tmpfile)
	      val journal_base = (getInfo "scs_print_journal") ^ "/"
	      val journal_dir = (Date.fmt "%Y"  (ScsDate.now_local())) ^ "/" ^ 
		(Date.fmt "%m"  (ScsDate.now_local())) ^ "/" ^ (StringCvt.padLeft #"0" 4 batch_id)
	      val _ = ScsFile.mkDir (journal_base ^ journal_dir)
	      val target_f = journal_dir ^ "/" ^ tmpfile ^ ".pdf"
	      val cmd = Quot.toString `cd ^(path_preview()); latex ^(texfile tmpfile); dvips -o ^(psfile tmpfile) ^(dvifile tmpfile); lpr -P^printer ^(psfile tmpfile); ps2pdf ^(psfile tmpfile) ^(pdffile tmpfile); mv ^(pdffile tmpfile) ^(journal_base ^ target_f)`
	      fun ins_log db =
		let
		  val clob_id = DbClob.insert_fn source db
		in
		  Db.Handle.dmlDb db `insert into scs_print_log (batch_id,print_id,user_id,category,clob_id,print_cmd,
				                          target_file,doc_type,note,deleted_p,
							  on_what_table, on_what_id, time_stamp)
			       values (^(Db.valueList [batch_id,print_id,Int.toString ScsLogin.user_id,
				                       category,clob_id,cmd,target_f,
						       docTypeToString doc_type,note,"f",
						       on_what_table,on_what_id]),
				       ^(Db.sysdateExp))`
		end
	    in
	      if Process.system cmd = Process.success
		then (ScsDb.panicDmlTrans ins_log;
		      ScsPage.returnPg (%"Document Printed")
		      (case ScsLogin.user_lang of
			 ScsLang.en => `The document is now sent to printer ^printer.<p>

                           The document has been filed. If there were any problems
                           printing the document then please 
                           <a href="toggle_deleted.sml?print_id=^(Ns.encodeUrl print_id)&target_url=^(
                           Ns.encodeUrl ("show_doc.sml?print_id="^print_id))">de-file</a> the
                           document. You will be returned to the print-screen again.`
                       | ScsLang.da => `Dokumentet er nu sendt til printer ^printer.<p>
                           
                           Dokumentet er journaliseret. Hvis der er problemer med udskriften, så skal
                           du <a href="toggle_deleted.sml?print_id=^(Ns.encodeUrl print_id)&target_url=^(
                           Ns.encodeUrl ("show_doc.sml?print_id="^print_id))">fjerne</a> dokumentet fra journalen igen.
                           Du vender da tilbage til udskriftssiden igen.`))

	      else ScsError.panic `ScsPrint.genTarget: Can't execute system command: ^cmd`
	    end

      fun printDoc' batch_id category note on_what_table on_what_id doc_type source printer =
	case doc_type of
	  LaTeX =>
	    let
	      val print_id = Int.toString (Db.seqNextval "scs_print_id_seq")
	      val tmpfile = ScsFile.uniqueFile (path_preview()) ^ "-" ^ print_id
	      val _ = ScsFile.save source (texfile tmpfile)
	      val journal_base = (getInfo "scs_print_journal") ^ "/"
	      val journal_dir = (Date.fmt "%Y"  (ScsDate.now_local())) ^ "/" ^ 
		(Date.fmt "%m"  (ScsDate.now_local())) ^ "/" ^ (StringCvt.padLeft #"0" 4 batch_id)
	      val _ = ScsFile.mkDir (journal_base ^ journal_dir)
	      val target_f = journal_dir ^ "/" ^ tmpfile ^ ".pdf"
	      val cmd = Quot.toString `cd ^(path_preview()); latex ^(texfile tmpfile); dvips -o ^(psfile tmpfile) ^(dvifile tmpfile); lpr -P^printer ^(psfile tmpfile); ps2pdf ^(psfile tmpfile) ^(pdffile tmpfile); mv ^(pdffile tmpfile) ^(journal_base ^ target_f)`
	      fun ins_log db =
		let
		  val clob_id = DbClob.insert_fn source db
		in
		  Db.Handle.dmlDb db `insert into scs_print_log (batch_id,print_id,user_id,category,clob_id,print_cmd,
				  	                  target_file,doc_type,note,deleted_p,
							  on_what_table, on_what_id, time_stamp)
			       values (^(Db.valueList [batch_id,print_id,Int.toString ScsLogin.user_id,
				 		       category,clob_id,cmd,target_f,
						       docTypeToString doc_type,note,"f",
						       on_what_table,on_what_id]),
				       ^(Db.sysdateExp))`
		end
	    in
	      if Process.system cmd = Process.success
		then (ScsDb.panicDmlTrans ins_log;
		      target_f)
	      else (ScsError.panic `ScsPrint.genTarget: Can't execute system command: ^cmd`;target_f)
	    end

      fun printDocs docs printer =
	let
	  val batch_id = Int.toString (Db.seqNextval "scs_print_batch_id_seq")
	  fun printDocs' ([],acc) = ScsPage.returnPg (%"Document Printed") `Dokumenter udskrevet`
	    | printDocs' ((category,note,on_what_table,on_what_id,doc_type,source)::xs,acc) = 
	    printDocs' (xs,printDoc' batch_id category note on_what_table on_what_id doc_type source printer :: acc)
	in
	  printDocs' (docs,[])
	end	  

      (* Should find printers for the user logged in *)
      fun choosePrinter n = (`Choose printer`, ScsWidget.select allPrinters n)

      fun printForm category note on_what_table on_what_id doc_type source =
	ScsWidget.formBox "/scs/print/scs-print.sml" 
	[("submit", "Print"),("submit","Update Source")] 
	(`You may change the source below and then either print the 
	 changed document or update the preview link.<p>` ^^ 
	 `<a href="^(genTarget doc_type source)">preview</a><br>` ^^
	 (Html.inhidden "doc_type" (docTypeToString doc_type)) ^^
	 (Html.inhidden "category" category) ^^
	 (Html.inhidden "on_what_table" on_what_table) ^^
	 (Html.inhidden "on_what_id" on_what_id) ^^
	 (Html.inhidden "note" note) ^^
	 (ScsWidget.largeTA "source" source) ^^ `<p>` ^^
	 (ScsWidget.oneLine (choosePrinter "printer")))
      end
  end