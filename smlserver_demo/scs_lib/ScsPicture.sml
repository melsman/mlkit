signature SCS_PICTURE =
  sig

    type picture_format = {size   : int, (* bytes *)
			   height : int,
			   width  : int,
			   magic  : string}

    (* [identifyPicture file] returns the format of a file
        representing a picture *)
    val identifyPicture : string -> picture_format option
    val identifyPictureErr : string * ScsFormVar.errs -> picture_format option * ScsFormVar.errs

    (* [direction] is used when rotating pictures. *)
    datatype direction = left | right
    val directionToString : direction -> string
    val directionFromString : string -> direction option
    val getDirectionErr : string * ScsFormVar.errs -> direction option * ScsFormVar.errs

    (* [rotatePicture (source,target,direction,errs)] rotates source
        picture into target picture. *)
    val rotatePicture : string * string * direction * ScsFormVar.errs -> ScsFormVar.errs

    (* [convertPicture (h,w,options,source_file,target_file,errs)]
        converts the source_file into target_file given the height h,
        width w and convert-options options. Add an error to errs if
        the picture an't be converted. *)
    val convertPicture : int * int * string * string * string * ScsFormVar.errs -> ScsFormVar.errs

    (* [ppPictureFormat pic] returns a pretty printing of the format. *)
    val ppPictureFormat : picture_format -> string

    (* [fixedHeight pic h] returns height and width of picture with
        format pic when the picture is scaled to height h. *)
    val fixedHeight : picture_format -> int -> int * int
  end

structure ScsPicture :> SCS_PICTURE =
  struct

    type picture_format = {size   : int, (* bytes *)
			   height : int,
			   width  : int,
			   magic  : string}

    fun identifyPicture file =
      let
	val cmd = Quot.toString 
	  `^(ScsConfig.scs_identify()) -format "size:%b-height:%h-width:%w-magic:%m" ^file `
	val regexp = RegExp.fromString "size:([0-9]+)(b|kb|mb)-height:([0-9]+)-width:([0-9]+)-magic:(.+)"
	val format = ScsFile.systemCmd cmd
	fun calcSize size bkbmb =
	  case bkbmb of
	    "b" => size
	  | "kb" => size * 1024
	  | "mb" => size * 1024 * 1024
	  | _ => raise Fail ("identifyPicture.calcSize: can't calculate size: " ^ bkbmb)
      in
	case RegExp.extract regexp (ScsString.lower format) of
	  NONE => NONE
	| SOME [size,bkbmb,height,width,magic] => 
	    SOME {size = calcSize ((Option.valOf o Int.fromString) size) bkbmb,
		  height = (Option.valOf o Int.fromString) height,
		  width = (Option.valOf o Int.fromString) width,
		  magic = magic}
	| _ => NONE
      end
    handle _ => NONE

    fun identifyPictureErr (file,errs) =
      case identifyPicture file of
	NONE => (NONE,ScsFormVar.addErr(ScsDict.s' [(ScsLang.da,`Filen kan ikke identificeres 
						     som et billede.`),
						    (ScsLang.en,`The file can't be identified 
						     as a picture.`)],
					errs))
      | SOME p => (SOME p, errs)

    datatype direction = left | right
    fun directionToString left = "left"
      | directionToString right = "right"
    fun directionFromString "left" = SOME left
      | directionFromString "right" = SOME right
      | directionFromString _ = NONE
    fun getDirectionErr (fv,errs) =
      let
	val err_dict = [(ScsLang.da,`Kan ikke rotere billede, da 
			 rotationsretning er ikke angivet.`),
			(ScsLang.en,`Can't rotate picture, because no
			 rotation-direction is given.`)]
      in
	case ScsFormVar.wrapOpt ScsFormVar.getStringErr fv of
	  SOME s =>
	    (case directionFromString s of
	       NONE => (NONE,ScsFormVar.addErr(ScsDict.s' err_dict,errs))
	     | SOME d => (SOME d,errs))
	| NONE => (NONE,ScsFormVar.addErr(ScsDict.s' err_dict,errs))
      end
    fun rotatePicture (source,target,direction,errs) =
      let
	val direc =
	  case direction of
	    left => "-90"
	    | right => "90"
	val cmd = Quot.toString
	  `^(ScsConfig.scs_convert()) -sharpen 3 -quality 100 -rotate ^(direc) ^source ^target`
      in
	ScsFile.systemCmd cmd;
	errs
      end
    handle _ => ScsFormVar.addErr(ScsDict.s' [(ScsLang.da, `Fejl ved rotation af billede.`),
					      (ScsLang.en, `Can't rotate picture.`)],errs)

    fun ppPictureFormat (pic:picture_format) = Quot.toString
      `size: ^(Int.toString (#size pic))
       height : ^(Int.toString (#height pic))
       width : ^(Int.toString (#width pic))
       magic : ^(#magic pic)`
       
    fun convertPicture (h,w,options,source_file,target_file,errs) =
      let
	val cmd = Quot.toString
	  `^(ScsConfig.scs_convert()) -geometry ^(Int.toString w)x^(Int.toString h) ^options ^source_file ^target_file`
      in
	ScsFile.systemCmd cmd;
	errs
      end
    handle _ => ScsFormVar.addErr(ScsDict.s' [(ScsLang.da, `Fejl ved behandling af billede.`),
					      (ScsLang.en, `Can't convert picture.`)],errs)

    fun fixedHeight (pic:picture_format) h =
      (h,Real.round(Real.fromInt (#width pic) / (Real.fromInt (#height pic)) * (Real.fromInt h)))
  end