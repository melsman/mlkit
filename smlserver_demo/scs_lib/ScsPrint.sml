signature SCS_PRINT =
  sig
    datatype doc_type = LaTeX
    val docTypeToString      : doc_type -> string
    val docTypeFromString    : string -> doc_type
    val allDocTypes          : doc_type list
    val ppAllDocTypes        : unit -> string list

    val allPrinters   : string list

    (* Widgets *)
    val choosePrinter : string -> quot * quot
    val printForm     : doc_type -> quot -> quot

    (* Actual Printing *)
    val printDoc      : doc_type -> string -> string -> Ns.status
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

    val allPrinters = ["p152","p177","p177d","p177t","p233","p233d","p233t"]
    (* Generate file which can be printed and previewed. *)
    (* Files are stored in the scs_print_dir directory.  *)
    local
      fun tmpnam (c: int) : string =
	if c > 10 then ScsError.panic `ScsPrint.tmpnam: Can't create temporary file`
	else
	  let val is = Random.rangelist (97,122) (8, Random.newgen())
	    val f = implode (map Char.chr is)
	  in if FileSys.access(f,[]) then tmpnam(c+1)
	     else f
	  end
      val basedir = "/web/ucs_prod/www/"
      val scs_print_dir = "scs_print/"
      fun texfile f = basedir ^ scs_print_dir ^ f ^ ".tex"
      fun psfile f = basedir ^ scs_print_dir ^ f ^ ".ps"
      fun pdffile f = basedir ^ scs_print_dir ^ f ^ ".pdf"
      fun dvifile f = basedir ^ scs_print_dir ^ f ^ ".dvi"
      fun pdfurl f = "/" ^ scs_print_dir ^ f ^ ".pdf"
      fun save_source source filename =
	let
	  val texstream = TextIO.openOut filename
	in
	  TextIO.output (texstream,source);
	  TextIO.closeOut texstream
	end
    in
      fun genTarget doc_type source =
	case doc_type of
	  LaTeX =>
	    let
	      val tmpfile = tmpnam 10
	      val _ = save_source (Quot.toString source) (texfile tmpfile)
	      val cmd = Quot.toString `cd ^scs_print_dir; latex ^(texfile tmpfile); dvips -o ^(psfile tmpfile) ^(dvifile tmpfile); ps2pdf ^(psfile tmpfile) ^(pdffile tmpfile)`
	    in
	      if Process.system cmd = Process.success
		then pdfurl tmpfile
	      else
		ScsError.panic `ScsPrint.genTarget: Can't execute system command: ^cmd`
	    end
      fun printDoc doc_type source printer =
	case doc_type of
	  LaTeX =>
	    let
	      val tmpfile = tmpnam 10
	      val _ = save_source source (texfile tmpfile)
	      val cmd = Quot.toString `cd ^scs_print_dir; latex ^(texfile tmpfile); dvips -P^printer ^(dvifile tmpfile)`
	    in
	      if Process.system cmd = Process.success
		then ScsPage.returnPg "Document Printed" `The document is now sent to printer ^printer.`
	      else ScsError.panic `ScsPrint.genTarget: Can't execute system command: ^cmd`
	    end

      (* Sould find printers for the user logged in *)
      fun choosePrinter n = (`Choose printer`, ScsWidget.select (List.map (fn p => (p,p)) allPrinters) n)

      fun printForm doc_type source =
	ScsWidget.formBox "/scs/scs-print.sml" 
	[("submit", "Print"),("submit","Update Source")] 
	(`You may change the source below and then either print the 
	 changed document or update the preview link.<p>` ^^ 
	 `<a href="^(genTarget doc_type source)">preview</a><br>` ^^
	 (Html.inhidden "doc_type" (docTypeToString doc_type)) ^^
	 (ScsWidget.largeTA "source" source) ^^ `<p>` ^^
	 (ScsWidget.oneLine (choosePrinter "printer")))
      end
  end