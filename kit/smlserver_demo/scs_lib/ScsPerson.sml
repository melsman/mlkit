signature SCS_PERSON =
  sig
    datatype sex = Female | Male

    type person_record = {
      person_id           : int,
      first_names 	  : string,
      last_name		  : string,
      name 		  : string,
      norm_name           : string,
      email		  : string,
      url		  : string,
      cpr                 : string,
      upload_folder_id    : int option, (* folder containing portraits *)
      upload_folder_name  : string option,
      upload_folder_path  : string option,
      may_show_portrait_p : bool
    }

    type profile_record = 
      { party_id       : int,
        profile_tid    : int,
        profile_da     : string,
        profile_en     : string,
	keywords_tid   : int,
	keywords_da    : string,
	keywords_en    : string,
	edit_no        : int,
	last_modified  : Date.date,
	modifying_user : int,
	deleted_p      : bool}

    datatype portrait_type = original | thumb_fixed_height
    type portrait_record =
      { file_id             : int,
        party_id            : int,
	portrait_type_vid   : int,
        portrait_type_val   : portrait_type,
	filename            : string,
	url                 : string,
	width               : int,
	height              : int,
	bytes               : int,
	official_p          : bool,
	person_name         : string,
	may_show_portrait_p : bool,
	modifying_user      : int,
	last_modified       : Date.date}

    datatype portrait_mode =
      PORTRAIT_ADM_HELP
    | PORTRAIT_ADM
    val getPortraitModeErr   : string * ScsFormVar.errs -> portrait_mode * ScsFormVar.errs
    val portraitModeToString : portrait_mode -> string
    val portraitFaneside     : portrait_mode -> int option -> 
                               quot option list -> portrait_mode UcsWidget.faneside option
    val portraitFaneblad     : portrait_mode -> portrait_mode UcsWidget.faneside option list -> quot

    datatype upload_mode =
      UPLOAD
    | ROTATE
    | DELETE 
    | MAY_SHOW_PORTRAIT
    | MAKE_NON_OFFICIAL_OFFICIAL
    val getUploadModeErr   : string * ScsFormVar.errs -> upload_mode * ScsFormVar.errs
    val uploadModeToString : upload_mode -> string

    val portrait_types_enum_name : string
    val portrait_type_from_DB : string -> portrait_type option
    val portrait_type_to_DB   : portrait_type -> string

    (* Default portraits *)
    val empty_portrait_thumbnail_da_filename : string
    val empty_portrait_thumbnail_en_filename : string
    val empty_portrait_large_da_filename     : string
    val empty_portrait_large_en_filename     : string
    val empty_portrait_urls                  : string list
    val empty_portrait_thumbnail             : unit -> portrait_record
    val empty_portrait_large                 : unit -> portrait_record

    (* [getPortrait file_id] returns the portrait represented by file_id. *)
    val getPortrait : int -> portrait_record option

    (* [getPortraits user_id] returns a list of portraits uploaded
        for this user. *)
    val getPortraits : int -> portrait_record list

    (* [getPicture pic_type official_p portraits] returns a picture of
       type pic_type that is official or non official depending on
       official_p if exists in the list portraits. *)
    val getPicture : portrait_type -> bool -> portrait_record list -> portrait_record option

    (* [delPortrait db pic] deletes picture pic from
        scs_portraits. *)
    val delPortrait : Db.Handle.db -> portrait_record -> unit

    (* [insPortrait db pic] inserts pic in scs_portraits. *)
    val insPortrait : Db.Handle.db -> portrait_record -> unit

    (* [mayReturnPortrait_p user_id file_id adm_p] returns true if portrait
       represented by file_id may be returned:

         * may_show_portrait_p is true
         * logged in user_id = person_id
         * adm_p is true. *)
    val mayReturnPortrait_p : int -> int -> bool -> bool
    
    (* [cacheMayReturnPortrait_p file_id person_id show_p] updates
        cached values of showing pictures to the public. *)
    val cacheMayReturnPortrait_p : int -> int -> bool -> unit

    (* [returnLargePortrait ()] expects fv person_id and returns the
    original picture for that person. Used in trap.sml *)
    val returnLargePortrait : unit -> unit

    (* [returnThumbnail ()] expects fv person_id and returns the
    thumbnail picture for that person. Used in trap.sml *)
    val returnThumbnail : unit -> unit

    (* [returnPortraitFile ()] returns the portrait file given by fv
        file_id. Used in trap.sml *)
    val returnPortraitFile : unit -> unit

    (* [mayToggleShowPortrait user_id] returns true if user_id may
        toggle the field may_show_portrait_p. *)
    val mayToggleShowPortrait : int -> bool

    (* [mayMakeNonOfficialOfficial user_id] returns true if user_id may
        make the non official portrait the official one. *)
    val mayMakeNonOfficialOfficial : int -> bool

    (* [mayKeepOrigSize user_id] returns true if user_id may keep the
        original size of the picture, that is, the size is not
        reduced. *)
    val mayKeepOrigSize : int -> bool

    (* [genPortraitHtml thumb_opt large_opt default] returns HTML
        for showing a portrait as a thumbnail and when clicked opens
        a larger portrait. If either thumb_opt or large_opt is NONE,
        then default is returned. *)
    val genPortraitHtml    : portrait_record option -> portrait_record option -> string -> string

    (* [genExtPortraitHtml thumb_opt large_opt default] returns HTML
        for showing a portrait from an external page as a thumbnail
        and when clicked opens a larger portrait. If either thumb_opt
        or large_opt is NONE, then default is returned. *)
    val genExtPortraitHtml : portrait_record option -> portrait_record option -> string -> string

    (* [portraitAsHtml (user_id,per:person_record,official_p,adm_p)]
       returns HTML for a portrait. user_id is the logged in user. per
       is the person for which we will show a portrait. If official_p
       is true, then we show the official portrait even if a
       non-official portrait exists. If adm_p is true then the
       portrait is shown even though the person has not allowed his
       portrait to be shown to every one on the Internet/Intranet. 
       
       The rules used to pick portrait:
          1 if may_show_portrait_p = false andalso 
               user_id <> person_id andalso 
               not adm_p then default_picture
          2 non official picture (if official_p = false)
          3 official picture *)
    val portraitAsHtml : int * person_record * bool * bool -> string

    (* [upload_portrait_label] is the root folder in the Scs File Storage
        system where portraits are uploaded. *)
    val upload_root_label : string
  
    (* [max_height] is the maximum height of original picture. If the
        uploaded picture is larger, then it is scaled down. *)
    val max_height   : int

    (* [scs_approvals_show_portrait_name] is the string used when
        loggin may_show_portrait_p in ScsApprovals. *)
    val scs_approvals_show_portrait_name : string

    (* [thumb_height] is the height of a thumbnail. *)
    val thumb_height : int

    (* [uploadPortraitPriv (user_id, official_p, per)] returns the
        priviledge that user_id has on uploading pictures (either
        officials og non officials) to person per. *)
    val uploadPortraitPriv : int * bool * person_record -> ScsFileStorage.priv

    val may_see_portrait_adm_p : int -> bool 

    (* [portrait_req_info] is a dictionary with info about pictures
        and the requirements *)
    val portrait_req_info : ScsDict.dict

    (* [editPicForm person_id target official_p thumb_opt large_opt]
        returns HTML for editing pictures thumb_opt and
        large_opt. Target is either user or adm and it controls the
        page to which we redirect. *)
    val editPicForm : int -> string -> bool -> 
      portrait_record option -> portrait_record option -> string

    (* [getOrCreateUploadFolderId db prj] returns an id on the folder
        in the file storage area where uploaded portraits to this person
        are stored. This function access and updates the database in
        case no previous folder path and name have been calculated. *)
    val getOrCreateUploadFolderId : Db.Handle.db * person_record -> int

    (* [getPerson user_id] fetches a person from the database *)
    val getPerson : int -> person_record option

    (* [getPersonByExtSource on_what_table on_which_id] fetches a
       person from the database that relates to the external source
       represented by on_what_table and on_which_id. *)
    val getPersonByExtSource : string -> int -> person_record option

    (* [getPersonErr (id,errs)] returns a person record if exists;
       otherwise an error is appended to the error list errs. *)
    val getPersonErr : int * ScsFormVar.errs -> person_record option * ScsFormVar.errs 

    (* [getProfile user_id] fetches a profile from the database. It
       only returns NONE if user_id does not exists in scs_parties. An
       empty profile is created for user_id if no one exists *)
    val getProfile : int -> profile_record option

    (* [getProfileErr (id,errs)] returns a profile record if exists;
       otherwise an error is appended to the error list errs. *)
    val getProfileErr : int * ScsFormVar.errs -> profile_record option * ScsFormVar.errs 

   (* [searchPerson pat keep_del_p] returns a list of persons matching
      the pattern pat. If deleted_p is true then we also search in
      deleted persons *)
    val searchPerson : string -> bool -> person_record list


    (* [nameToHtml name email] returns HTML for a name linking to
        email *)
    val nameToHtml : string * string -> string

    (* [search_form target_url hvs] generates a standard HTML search
       form. The user enter a search expression (e.g., name, security
       id) and either

         * the search expression does not identity any person and an
           info message is shown to the user.

         * the search expression identify exactly one person, and a
           redirect to the page represented by target_url is issued
           with hidden variables hvs and user_id = x where x is
           user_id for the person found.

         * the search expression identity more than one person in
           which case the user is presented a page on which she slects
           the person she seeks. *)
    val search_form: string -> (string*string) list -> quot

    (* [getPersonIdErr fv errs] checks that fv is an integer which can
       be used as a person_id. The database is not checked. See also
       ScsFormVar.sml *)
    val getPersonIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

   (* [getOfficialpErr fv errs] checks that fv is a bool. The database
       is not checked. See also ScsFormVar.sml *)
    val getOfficialpErr : string * ScsFormVar.errs -> bool * ScsFormVar.errs

   (* [getMayShowPortraitpErr (fv,errs)] checks that fv is a bool. *)
    val getMayShowPortraitpErr : string * ScsFormVar.errs -> bool * ScsFormVar.errs

    (* [splitCpr cpr] if cpr is on the form xxxxxxyyyy then the pair
       (xxxxxx,yyyy) is returned. *)
    val splitCpr      : string -> (string*string)

    (* [ppCpr cpr]  if cpr is on the form xxxxxxyyyy then the string
       xxxxxx-yyyy is returned; otherwise the argument is returned. *)
    val ppCpr : string -> string

    (* [makeCprPublic cpr] if cpr in on the form aaaaaabbbb then the
       revised cpr aaaaaaXXXX is returned *)
    val makeCprPublic : string -> string

    (* [cprToDate cpr] if cpr is a valid cpr then a date is returned
       representing the birth date. If cpr is not valid then an
       exception is raised. *)
    val cprToDate     : string -> Date.date

    (* [cprToSex cpr] if cpr is a valid cpr then the sex (either Male
       of Female) is returned. Otherwise an exception is returned. *)
    val cprToSex : string -> sex

    (* [isFemale_p cpr] returns true if the cpr is female. Throws an 
	exception if cpr is invalid *)
    val isFemale_p : string -> bool

    (* [fix_email email] do the following conversions:
         - if email is of form login@it-c.dk => login@itu.dk
         - if email is of form login@it.edu => login@itu.dk
         - if email is of form login => login@itu.dk
     *)
    val fix_email : string -> string

    val portrait_adm_nb : string option -> UcsPage.navbar_item
    val portrait_adm_help : quot
  end

structure ScsPerson :> SCS_PERSON =
  struct
(*
    val name = ScsPersonData.name
*)
    datatype sex = Female | Male

    type person_record = {
      person_id           : int,
      first_names 	  : string,
      last_name		  : string,
      name 		  : string,
      norm_name           : string,
      email		  : string,
      url		  : string,
      cpr                 : string,
      upload_folder_id    : int option, (* folder containing portraits *)
      upload_folder_name  : string option,
      upload_folder_path  : string option,
      may_show_portrait_p : bool
    }

    type profile_record = 
      { party_id       : int,
        profile_tid    : int,
        profile_da     : string,
        profile_en     : string,
	keywords_tid   : int,
	keywords_da    : string,
	keywords_en    : string,
	edit_no        : int,
	last_modified  : Date.date,
	modifying_user : int,
	deleted_p      : bool}

    datatype portrait_type = original | thumb_fixed_height
    type portrait_record =
      { file_id             : int,
        party_id            : int,
	portrait_type_vid   : int,
        portrait_type_val   : portrait_type,
	filename            : string,
        url                 : string,
	width               : int,
	height              : int,
	bytes               : int,
	official_p          : bool,
	person_name         : string,
        may_show_portrait_p : bool,
	modifying_user      : int,
	last_modified       : Date.date}

    val portrait_types_enum_name = "scs_portrait_types"
    fun portrait_type_from_DB "orig" = SOME original
      | portrait_type_from_DB "thumb_fixed_height" = SOME thumb_fixed_height
      | portrait_type_from_DB "" = NONE
      | portrait_type_from_DB s = ScsError.panic `ScsPerson.protrait_type_from_DB: can't convert ^s`
    fun portrait_type_to_DB original = "orig"
      | portrait_type_to_DB thumb_fixed_height = "thumb_fixed_height"

    datatype portrait_mode =
      PORTRAIT_ADM_HELP
    | PORTRAIT_ADM

    fun getPortraitModeErr (mode_fv,errs) = 
      case ScsFormVar.wrapOpt ScsFormVar.getStringErr mode_fv of
	SOME s =>
	  (case s of
	     "portrait_adm_help" => (PORTRAIT_ADM_HELP,errs)
	   | "portrait_adm" => (PORTRAIT_ADM,errs)
	   | _ => (PORTRAIT_ADM,errs)) (* Default we use PORTRAIT_ADM *)
      | NONE => (PORTRAIT_ADM,errs) (* Default we use PORTRAIT_ADM *)

    fun portraitModeToString m =
      case m of
	PORTRAIT_ADM_HELP => "portrait_adm_help"
      | PORTRAIT_ADM => "portrait_adm"

    fun portraitFaneside mode person_id_opt body =
      let
	val hvs = 
	  case person_id_opt of
	    SOME person_id => [("person_id",Int.toString person_id)]
	  | NONE => []
      in
	case mode of
	  PORTRAIT_ADM =>
	    SOME (UcsDict.portrait_adm_dict, Html.genUrl "/scs/person/portrait_adm_form.sml" 
		  (("mode", portraitModeToString PORTRAIT_ADM)::hvs), PORTRAIT_ADM, body)
	| PORTRAIT_ADM_HELP => 
	    SOME (UcsDict.portrait_adm_help_dict, Html.genUrl "/scs/person/portrait_adm_form.sml" 
		  (("mode", portraitModeToString PORTRAIT_ADM_HELP)::hvs), PORTRAIT_ADM_HELP, body)
      end

    local
      fun outerBox top qs =
	let
	  val (url,mine_dict) = ("/", UcsDict.my_page_dict)
	in
	  UcsWidget.outerBox_template (url, mine_dict) top qs 
	end
    in
      fun portraitFaneblad mode fanesider = 
	UcsWidget.faneblad_template mode fanesider outerBox
    end

    datatype upload_mode =
      UPLOAD
    | ROTATE
    | DELETE 
    | MAY_SHOW_PORTRAIT
    | MAKE_NON_OFFICIAL_OFFICIAL

    fun getUploadModeErr (mode_fv,errs) = 
      case ScsFormVar.wrapOpt ScsFormVar.getStringErr mode_fv of
	SOME s =>
	  (case s of
	     "upload" => (UPLOAD,errs)
	   | "rotate" => (ROTATE,errs)
	   | "delete" => (DELETE,errs)
	   | "may_show_portrait" => (MAY_SHOW_PORTRAIT,errs)
	   | "make_non_official_official" => (MAKE_NON_OFFICIAL_OFFICIAL,errs)
	   | _ => (UPLOAD,errs)) (* Default we use UPLOAD *)
      | NONE => (UPLOAD,errs) (* Default we use PORTRAIT_ADM *)

    fun uploadModeToString mode = 
      case mode of
	UPLOAD => "upload"
      | ROTATE => "rotate"
      | DELETE => "delete"
      | MAY_SHOW_PORTRAIT => "may_show_portrait"
      | MAKE_NON_OFFICIAL_OFFICIAL => "make_non_official_official"


   (* Check for form variables *)
    fun getPersonIdErr (fv,errs) = ScsFormVar.getIntErr(fv,"Person id",errs)

    fun getOfficialpErr (fv,errs) = ScsFormVar.getBoolErr(fv,"Official_p",errs)

    fun getMayShowPortraitpErr (fv,errs) = ScsFormVar.getBoolErr(fv,"May show portrait",errs)

    val upload_root_label = "ScsPersonPortrait"        (* Upload root label used in FS *)
    val max_height = 400                               (* Height of large pictures *)
    val thumb_height = 200                             (* Height of thumbnails *)
    val download_dir = "/scs/person/portrait_download" (* Virtually download directory *)
    val scs_approvals_show_portrait_name ="scs_parties:may_show_portrait"
    local
      fun mk_url (filename,file_id) = 
	Html.genUrl (download_dir ^ "/" ^ filename) [("file_id",Int.toString file_id)]
      fun f g = 
	let
	  val file_id = (ScsError.valOf o Int.fromString) (g "file_id")
	  val filename = g "filename"
	in
	  {file_id = file_id,
	   party_id = (ScsError.valOf o Int.fromString) (g "party_id"), 
	   portrait_type_vid = (ScsError.valOf o Int.fromString) (g "portrait_type_vid"), 
	   portrait_type_val = (ScsError.valOf o portrait_type_from_DB) (g "portrait_type_val"),
	   filename = filename,
	   url = mk_url (filename,file_id),
	   width = (ScsError.valOf o Int.fromString) (g "width"),
	   height = (ScsError.valOf o Int.fromString) (g "height"),
	   bytes = (ScsError.valOf o Int.fromString) (g "bytes"),
	   official_p = (ScsError.valOf o Db.toBool) (g "official_p"),
	   person_name = g "person_name",
	   may_show_portrait_p = (ScsError.valOf o Db.toBool) (g "may_show_portrait_p"),
	   modifying_user = (ScsError.valOf o Int.fromString) (g "modifying_user"),
	   last_modified = (ScsError.valOf o Db.toTimestamp) (g "last_modified")}
	end
      fun portraitSQL from_wh = 
	` select p.file_id,
                 p.party_id,
		 scs_person.name(p.party_id) as person_name,
                 p.portrait_type_vid,
                 scs_enumeration.getVal(p.portrait_type_vid) as portrait_type_val,
		 fs.name as filename,
                 p.width,
                 p.height,
                 p.bytes,
                 p.official_p,
                 party.may_show_portrait_p,
                 p.modifying_user,
                 ^(Db.toTimestampExp "p.last_modified") as last_modified
	   ` ^^ from_wh
    in
      fun getPortrait file_id =
	SOME (Db.oneRow' f (portraitSQL ` from scs_portraits p, scs_fs_files fs, scs_parties party
		                         where p.file_id = '^(Int.toString file_id)'
                                           and p.file_id = fs.file_id 
                                           and p.party_id = party.party_id`))
	handle _ => NONE
      fun getPortraits user_id =
	ScsError.wrapPanic (Db.list f) (portraitSQL ` from scs_portraits p, scs_fs_files fs, 
					                   scs_parties party
                                                     where p.party_id = '^(Int.toString user_id)'
                                                       and p.file_id = fs.file_id
                                                       and p.party_id = party.party_id`)
    end

    val empty_portrait_thumbnail_da_filename = "empty_portrait_thumbnail_da.jpg"
    val empty_portrait_thumbnail_en_filename = "empty_portrait_thumbnail_en.jpg"
    val empty_portrait_large_da_filename = "empty_portrait_large_da.jpg"
    val empty_portrait_large_en_filename = "empty_portrait_large_en.jpg"
    val empty_portrait_urls = 
      List.map (fn f => download_dir ^ "/" ^ f) 
      [empty_portrait_thumbnail_da_filename,
       empty_portrait_thumbnail_en_filename,
       empty_portrait_large_da_filename,
       empty_portrait_large_en_filename]
    fun empty_portrait_thumbnail () : portrait_record = 
      let
	val (filename,url,bytes) =
	  case ScsLogin.user_lang() of
	    ScsLang.da => (empty_portrait_thumbnail_da_filename,"/"^empty_portrait_thumbnail_da_filename,2954)
	  | ScsLang.en => (empty_portrait_thumbnail_en_filename,"/"^empty_portrait_thumbnail_en_filename,2676)
      in
	{file_id = 0,
	 party_id = 0,
	 portrait_type_vid = 0,
	 portrait_type_val = thumb_fixed_height,
	 filename = filename,
	 url = download_dir ^ url,
	 width = 170,
	 height = 200,
	 bytes = bytes,
	 official_p = false,
	 person_name = "-",
	 may_show_portrait_p = true,
	 modifying_user = 0 (* Arbitrary value *),
	 last_modified = ScsDate.now_local() (* Arbitrary value *)}
      end
    fun empty_portrait_large () : portrait_record =
      let
	val (filename,url,bytes) =
	  case ScsLogin.user_lang() of
	    ScsLang.da => (empty_portrait_large_da_filename,"/"^empty_portrait_large_da_filename,7255)
	  | ScsLang.en => (empty_portrait_large_en_filename,"/"^empty_portrait_large_en_filename,6793)
      in
	{file_id = 0,
	 party_id = 0,
	 portrait_type_vid = 0,
	 portrait_type_val = original,
	 filename = filename,
	 url = download_dir ^ url,
	 width = 340,
	 height = 400,
	 bytes = bytes,
	 official_p = false,
	 person_name = "-",
	 may_show_portrait_p = true,
	 modifying_user = 0 (* Arbitrary value *),
	 last_modified = ScsDate.now_local() (* Arbitrary value *)}
      end

    fun getPicture pic_type official_p (portraits : portrait_record list) = 
      List.find (fn pic => #portrait_type_val pic = pic_type andalso 
		 #official_p pic = official_p) portraits

    fun delPortrait db (pic : portrait_record) =
      let
	val del_sql = `delete from scs_portraits
                        where file_id = '^(Int.toString (#file_id pic))'`
      in
	ScsError.wrapPanic
	(Db.Handle.dmlDb db) del_sql
      end	

    fun insPortrait db (pic : portrait_record) =
      let
	val ins_sql = `insert into scs_portraits
	                 (file_id,party_id,portrait_type_vid,width,height,bytes,official_p,modifying_user)
                       values
		         ('^(Int.toString (#file_id pic))','^(Int.toString (#party_id pic))',
			  scs_enumeration.getVID(^(Db.qqq portrait_types_enum_name),
						 '^(portrait_type_to_DB (#portrait_type_val pic))'),
                          '^(Int.toString (#width pic))', '^(Int.toString (#height pic))',
			  '^(Int.toString (#bytes pic))', '^(Db.fromBool (#official_p pic))',
			  '^(Int.toString (ScsLogin.user_id()))')`
      in
	ScsError.wrapPanic
	(Db.Handle.dmlDb db) ins_sql
      end

    local
      val may_return_portrait_cache =
	Ns.Cache.get(Ns.Cache.Int,
		     Ns.Cache.Pair Ns.Cache.Bool Ns.Cache.Int,
		     "ScsPersonRetPortrait",
		     Ns.Cache.TimeOut 80000)
      fun may_show_portrait_p' file_id =
	case getPortrait file_id of
	  SOME pic => (#may_show_portrait_p pic,#party_id pic)
	| NONE => (false,~1) (* No picture so do not return it. *)
      fun may_show_portrait_p file_id =	
	Ns.Cache.memoize may_return_portrait_cache may_show_portrait_p' file_id
    in
      fun mayReturnPortrait_p user_id file_id adm_p = 
	let
	  val (may_show_p,party_id) = may_show_portrait_p file_id 
	in 
	  may_show_p orelse
	  user_id = party_id orelse
	  adm_p
	end
      fun cacheMayReturnPortrait_p file_id person_id show_p =
	(Ns.Cache.insert (may_return_portrait_cache,file_id,(show_p,person_id));())
    end

    (* Supporting links to pictures from external pages. *)
    fun may_see_portrait_adm_p user_id = ScsRole.has_one_p user_id [ScsRole.PhdAdm,ScsRole.StudAdm,
								    ScsRole.UcsEduInfo,ScsRole.PortraitAdm]
    local 
      fun returnEmpty pic_type = 
	case pic_type of 
	  thumb_fixed_height => 
	    (Ns.returnFile (Ns.Info.pageRoot() ^ (#url (empty_portrait_thumbnail())));())
	| _ => 
	    (Ns.returnFile (Ns.Info.pageRoot() ^ (#url (empty_portrait_large())));())
      fun returnFile pic_type user_id file_id =
	if mayReturnPortrait_p user_id file_id (may_see_portrait_adm_p user_id) then
	   ScsFileStorage.returnFile upload_root_label file_id
	else
	   returnEmpty pic_type

      fun returnPic pic_type =
	let
	  val user_id = ScsLogin.user_id() (* User is not necessarily logged in! *)
	  val (person_id,errs) = getPersonIdErr("person_id",ScsFormVar.emptyErr)
	in
	  if ScsFormVar.isErrors errs then
	    returnEmpty pic_type
	  else
	    let
	      val portraits = getPortraits person_id
	    in
	      case getPicture pic_type false portraits of
		SOME pic => returnFile pic_type user_id (#file_id pic) (* Non official pictures first. *)
	      | NONE => 
		  (case getPicture pic_type true portraits of
		     SOME pic => returnFile pic_type user_id (#file_id pic) (* Official picture last. *)
		   | NONE => returnEmpty pic_type)
	    end
	end
    in
      fun returnLargePortrait () = returnPic original
      fun returnThumbnail () = returnPic thumb_fixed_height
      fun returnPortraitFile () =
	let
	  val user_id = ScsLogin.user_id() (* User is not necessarily logged in! *)
	  val (file_id,errs) = ScsFileStorage.getFileIdErr("file_id",ScsFormVar.emptyErr)
	in
	  if ScsFormVar.isErrors errs then
	    returnEmpty thumb_fixed_height
	  else
	    returnFile thumb_fixed_height user_id file_id
	end
      fun genExtPortraitHtml (thumb_opt:portrait_record option) (large_opt:portrait_record option) default = 
	let
	  fun fileextension filename = 
	    case Path.ext filename of
	      NONE => ""
	    | SOME ext => ext
	in
	  case (thumb_opt,large_opt) of
	    (SOME thumb,SOME large) => 
	      Quot.toString
	      `<a href="^(Html.genUrl (Quot.toString 
                                       `^(Ns.Conn.location())^(download_dir)/portrait.^(fileextension (#filename large))`) 
                          [("person_id",Int.toString (#party_id large))])">
	      <img src="^(Html.genUrl (Quot.toString 
					`^(Ns.Conn.location())^(download_dir)/thumbnail.^(fileextension (#filename thumb))`) 
                          [("person_id",Int.toString (#party_id thumb))])"
	      alt="^(#person_name thumb)"></a>`
	  | _ => default
	end
    end

    fun mayToggleShowPortrait user_id =
      ScsRole.has_one_p user_id [ScsRole.SiteAdm,ScsRole.PortraitAdm]

    fun mayMakeNonOfficialOfficial user_id =
      ScsRole.has_one_p user_id [ScsRole.SiteAdm,ScsRole.PortraitAdm]

    fun mayKeepOrigSize user_id = 
      ScsRole.has_one_p user_id [ScsRole.SiteAdm,ScsRole.PortraitAdm]

    fun genPortraitHtml (thumb_opt:portrait_record option) (large_opt:portrait_record option) default = 
      case (thumb_opt,large_opt) of
	(SOME thumb,SOME large) => 
	  Quot.toString
	  `<a href="^(#url large)">
	   <img src="^(#url thumb)" 
	   width="^(Int.toString (#width thumb))" 
	   height="^(Int.toString (#height thumb))" 
	   align="right" alt="^(#person_name thumb)"></a>`
      | _ => default

    fun portraitAsHtml (user_id,per:person_record,official_p,adm_p) =
      let
	val default_html = genPortraitHtml 
	  (SOME (empty_portrait_thumbnail())) 
	  (SOME (empty_portrait_large())) ""
	val use_default_p =
	  #may_show_portrait_p per = false andalso
	  user_id <> #person_id per andalso not adm_p
	val portraits = getPortraits (#person_id per)
	val official_html = genPortraitHtml
	  (getPicture thumb_fixed_height true portraits) (getPicture original true portraits) default_html
	val non_official_html = 
	  if official_p then
	    official_html
	  else
	    genPortraitHtml (getPicture thumb_fixed_height false portraits)
	    (getPicture original false portraits) official_html
      in
	if use_default_p then
	  default_html
	else
	  non_official_html
      end	

    (* Priv rules:
         PortraitAdm = admin
         Person may edit non official picture and read official picture 
         Other persons may read both official and non official pictures
         if may_show_portrait_p is true *)
    fun uploadPortraitPriv (user_id, official_p, per:person_record) =
      if ScsRole.has_one_p user_id [ScsRole.PortraitAdm,ScsRole.SiteAdm] then ScsFileStorage.admin 
      else if #person_id per = user_id andalso not official_p then ScsFileStorage.read_add_delete
	   else if #person_id per = user_id then ScsFileStorage.read
		else if #may_show_portrait_p per then ScsFileStorage.read
		     else ScsFileStorage.no_priv

    val portrait_req_info = 
      [(ScsLang.da,`Du kan uploade et billede af dig selv som skal opfylde 
	følgende:
	<ul>
	<li> Maximal fil-størrelse er 300Kb.
	<li> Være af type <b>png</b>, <b>jpg</b> eller <b>gif</b>.
	<li> Forholdet mellem højde og bredde skal være mellem 1 og 2, 
	dvs. det må ikke være et panoramabillede.
	</ul>`),
       (ScsLang.en,`You may upload a picture of your self. The picture must 
	fulfill the following requirements:
	<ul>
	<li> Maximal file size is 300Kb.
	<li> The file type must be one of the following: <b>png</b>, 
	<b>jpg</b> or  <b>gif</b>.
	<li> The proportion between height and width must be between 1 and 2, 
	that is, it may not be a panorama picture.
	</ul>`)]

    fun editPicForm person_id target official_p
      (thumb_opt : portrait_record option) (large_opt : portrait_record option) =
      let
	val pic_exists_p = Option.isSome thumb_opt andalso Option.isSome large_opt
	val current_picture_html = genPortraitHtml thumb_opt large_opt ""
	val fv_target = Quot.toString `<input type=hidden name="target" value="^target">`
        val fv_official_p = Quot.toString `<input type=hidden name="official_p" 
	  value="^(Db.fromBool official_p)">`
	val confirm_upload = 
	  if pic_exists_p then
	    SOME (UcsPage.confirmOnClick (ScsDict.s [(ScsLang.da,`Bekræft, at du ønsker at udskifte det nuværende billede.`),
						     (ScsLang.en,`Please confirm, that you want to replace the 
						      current picture.`)]))
	  else
	    NONE
	val info_upload_file =
	  case thumb_opt of
	    SOME thumb => 
	      UcsPage.info
	      (ScsDict.s [(ScsLang.da,`<h2>Nuværende billede</h2>
			   Nuværende billede er gemt af ^(ScsPersonData.name (#modifying_user thumb)) den 
			   ^(ScsDate.ppTimestamp (#last_modified thumb)).`),
			  (ScsLang.en,`<h2>Current picture.</h2>
			   Current picture is uploaded by ^(ScsPersonData.name (#modifying_user thumb)) on
			   ^(ScsDate.ppTimestamp (#last_modified thumb))`)])
	  | NONE => ""
	val upload_form = `^(current_picture_html) ` ^^
	  (ScsWidget.namedBox "#999999" "#FFFFFF" (ScsDict.s [(ScsLang.en,`Upload picture`),
							     (ScsLang.da,`Upload billede`)]) 
	   ` 
	  <form enctype=multipart/form-data method=post action="/scs/person/upload_portrait.sml">
	  <input type=hidden name="person_id" value="^(Int.toString person_id)">
	  ^fv_target
	  ^fv_official_p
	  <input type=file size="20" name="upload_filename">
	  ^(UcsPage.mk_submit("submit",ScsDict.s [(ScsLang.da,`Gem fil`),
						  (ScsLang.en,`Upload file`)],confirm_upload))
	   ^info_upload_file
	  </form>`)


	val confirm_rotate =
	  SOME (UcsPage.confirmOnClick (ScsDict.s [(ScsLang.da,`Bekræft, at du ønsker at rotere det nuværende 
						    billede.`),
						   (ScsLang.en,`Please confirm, that you want to rotate the 
						    current picture.`)]))
	val rotate_form =
	  if pic_exists_p then
	    ScsWidget.namedBox "#999999" "#FFFFFF" (ScsDict.s [(ScsLang.en,`Rotate picture`),
							       (ScsLang.da,`Roter billede`)])
	    `<form action="/scs/person/upload_portrait.sml">
	    <input type=hidden name="mode" value="rotate"> 
  	    ^fv_target
   	    ^fv_official_p
	    <input type=hidden name="person_id" value="^(Int.toString person_id)">
	    <ul>
	    <li>^(ScsDict.s [(ScsLang.da,`roter billede til venstre`),
			 (ScsLang.en,`rotate picture to the left`)])
	    <input type=radio name="direction" 
	    value="^(ScsPicture.directionToString ScsPicture.left)">
	    <li>^(ScsDict.s [(ScsLang.da,`roter billede til højre`),
			 (ScsLang.en,`rotate picture to the right`)])
	    <input type=radio name="direction" 
	    value="^(ScsPicture.directionToString ScsPicture.right)"><br>
	    ^(UcsPage.mk_submit("submit",ScsDict.s [(ScsLang.da,`Roter`),
						    (ScsLang.en,`Rotate`)],confirm_rotate))
            </ul>
	    </form>
	    ^(ScsDict.s [(ScsLang.da,`Bemærk, at billedkvaliteten forringes en anelse ved 
			  rotation, så du skal ikke gøre dette mere end 1 eller 2 gange.`),
			 (ScsLang.en,`Notice, the picture quality reduces slightly by a 
			  rotation, so you should only do this once or twice.`)])`
	  else
	    ``

	val confirm_delete =
	  SOME (UcsPage.confirmOnClick (ScsDict.s [(ScsLang.da,`Bekræft, at du ønsker at slette det nuværende 
						    billede.`),
						   (ScsLang.en,`Please confirm, that you want to delete the 
						    current picture.`)]))
        val delete_form = 
	  if pic_exists_p then
	    ScsWidget.namedBox "#999999" "#FFFFFF" (ScsDict.s [(ScsLang.en,`Delete picture`),
							       (ScsLang.da,`Slet billede`)]) `
	    <form action="/scs/person/upload_portrait.sml">
	    <input type=hidden name="mode" value="delete"> 
	    ^fv_target
	    ^fv_official_p
	    <input type=hidden name="person_id" value="^(Int.toString person_id)">
	    ^(UcsPage.mk_submit("submit",ScsDict.s [(ScsLang.da,`Slet billede`),
						    (ScsLang.en,`Delete picture`)],confirm_delete))
	    </form>
	    `
	  else
	    ``

        val ext_link =
	  if pic_exists_p andalso #may_show_portrait_p (Option.valOf thumb_opt) then
	    ScsWidget.namedBox "#999999" "#FFFFFF" (ScsDict.s [(ScsLang.en,`Ekstern link til billede`),
							       (ScsLang.da,`External link to picture`)]) `
	    ^(ScsDict.s [(ScsLang.da,`Du kan anvende nedenstående HTML kode når du linker til billede eksternt:`),
			 (ScsLang.en,`You may use the HTML code below when you link to the picture from an
			  external document.`)])<p>
	    <code>
	    ^(ScsSecurity.xssFilterLeaveNoTags (genExtPortraitHtml thumb_opt large_opt ""))
	    </code>`
	  else
	    ``
      in
	Quot.toString (upload_form ^^ `<p>` ^^ rotate_form ^^ `<p>` ^^ delete_form ^^ `<p>` ^^ ext_link)
      end

    (* getOrCreateUploadFolderId: check folder_id in scs_parties and
       create a folder if none exists. The folder path is calculated
       as 
          t000/h00/party_id 
       where 
          party_id = thxx (t is for thousand and h for hundred)
    *)
    fun getOrCreateUploadFolderId (db:Db.Handle.db,per:person_record) =
      case #upload_folder_id per of
	SOME id => id
      | NONE =>
	  let
	    val div1000 = Int.div (#person_id per,1000)
	    val div100 = Int.div (#person_id per,100)
	    val folder_path = Int.toString div1000 ^ "/" ^ (Int.toString div100) ^ "/"
	    val folder_name = Int.toString (#person_id per) ^ "-" ^ (ScsFile.encodeFileNameUnix (#norm_name per))
	    val folder_id = 
	      ScsFileStorage.getOrCreateFolderId(db, upload_root_label, folder_path ^ folder_name)
	    val _ = 
	      ScsError.wrapPanic
	      (Db.Handle.dmlDb db)
	      `update scs_parties
                  set ^(Db.setList [("upload_folder_id", Int.toString folder_id),
				    ("upload_folder_path", folder_path),
				    ("upload_folder_name", folder_name)])
                where party_id = '^(Int.toString (#person_id per))'`
	  in
	    folder_id
	  end

    local
      fun f g = {person_id = (ScsError.valOf o Int.fromString) (g "person_id"),
		 first_names = g "first_names",
		 last_name = g "last_name",
		 name = g "name",
		 norm_name = g "norm_name",
		 email = g "email",
		 url = g "url",
		 cpr = g "cpr",
                 upload_folder_id = Int.fromString (g "upload_folder_id"),
		 upload_folder_name = 
  		   if String.size (g "upload_folder_name") = 0 then 
		     NONE 
		   else
		     SOME (g "upload_folder_name"),
		 upload_folder_path = 
  		   if String.size (g "upload_folder_path") = 0 then 
		     NONE 
		   else
		     SOME (g "upload_folder_path"),
		 may_show_portrait_p = (ScsError.valOf o Db.toBool) (g "may_show_portrait_p")}
      fun personSQL from_wh =
	` select p.person_id, p.first_names, p.last_name, 
		 scs_person.name(p.person_id) as name, 
                 p.norm_name,
		 party.email as email,
		 party.url as url,
		 p.security_id as cpr,
                 party.upload_folder_id,
                 party.upload_folder_name,
                 party.upload_folder_path,
                 party.may_show_portrait_p
            ` ^^ from_wh
    in
      fun getPerson user_id = 
	SOME( Db.oneRow' f (personSQL ` from scs_persons p, scs_parties party
                                       where person_id = '^(Int.toString user_id)'
                                         and person_id = party_id`))
	handle _ => NONE
      fun getPersonErr (user_id,errs) =
	case getPerson user_id of
	  NONE => 
	    let
	      val err_msg = [(ScsLang.da,`Personen du søger findes ikke.`),
			     (ScsLang.en,`The person you are seeking does not exits.`)]
	    in
	      (NONE,ScsFormVar.addErr(ScsDict.s' err_msg,errs))
	    end
	| p => (p,errs)
      fun getPersonByExtSource on_what_table on_which_id =
	SOME( Db.oneRow' f (personSQL ` from scs_persons p, scs_person_rels r, scs_parties party
                                       where r.on_what_table = ^(Db.qqq on_what_table)
                                         and r.on_which_id = '^(Int.toString on_which_id)'
                                         and r.person_id = p.person_id
                                         and p.person_id = party.party_id`))
	handle _ => NONE
      fun searchPerson pat keep_del_p =
	Db.list f (personSQL 
		   ` from scs_persons p, scs_parties party
                    where (lower(scs_person.name(p.person_id)) like ^(Db.qqq pat)
                       or lower(scs_party.email(p.person_id)) like ^(Db.qqq pat)
		       or p.security_id like ^(Db.qqq pat))
                      and p.deleted_p in (^(if keep_del_p then "'t','f'" else "'f'"))
                      and p.person_id = party.party_id`)
	handle _ => []
    end

    local
      fun f g = {party_id = (ScsError.valOf o Int.fromString) (g "party_id"),
		 profile_tid = (ScsError.valOf o Int.fromString) (g "profile_tid"),
		 profile_da = g "profile_da",
		 profile_en = g "profile_en",
		 keywords_tid = (ScsError.valOf o Int.fromString) (g "keywords_tid"),
		 keywords_da = g "keywords_da",
		 keywords_en = g "keywords_en",
		 edit_no = (ScsError.valOf o Int.fromString) (g "edit_no"),
		 last_modified = (ScsError.valOf o Db.toTimestamp) (g "last_modified"),
		 modifying_user = (ScsError.valOf o Int.fromString) (g "modifying_user"),
		 deleted_p = (ScsError.valOf o Db.toBool) (g "deleted_p")}
      fun profileSQL from_wh =
	` select p.party_id, 
                 p.profile_tid, 
		 scs_text.getText(p.profile_tid,'^(ScsLang.toString ScsLang.da)') 
		   as profile_da,
		 scs_text.getText(p.profile_tid,'^(ScsLang.toString ScsLang.en)') 
		   as profile_en,
                 p.keywords_tid, 
		 scs_text.getText(p.keywords_tid,'^(ScsLang.toString ScsLang.da)') 
		   as keywords_da,
		 scs_text.getText(p.keywords_tid,'^(ScsLang.toString ScsLang.en)') 
		   as keywords_en,
                 p.edit_no, 
                 ^(Db.toTimestampExp "p.last_modified") as last_modified, 
                 p.modifying_user, 
                 p.deleted_p
            ` ^^ from_wh
    in
      fun getProfile user_id =
	SOME(Db.oneRow' f (profileSQL ` from scs_profiles_w p
			               where p.party_id = '^(Int.toString user_id)'`))
	handle _ => 
	  (* Profile does not exits - so try to insert empty profile *)
	  let
	    val per_opt = getPerson user_id 
	  in
	    case per_opt of
	      NONE => NONE (* User does not exists *)
	    | SOME per => (* User exists so insert empty profile *)
		let
		  (* We set the creating user to be the not logged in user - pretty much arbitrarily *)
                  (* The current user may not be logged in. (e.g., if he comes from Find Person)     *)
		  fun new db =
		    let
		      val profile_tid = Db.Handle.oneFieldDb db `select scs.new_obj_id from dual`
		      val _ = Db.Handle.execSpDb db  
			[`scs_text.updateTextProc(text_id => ^profile_tid,language => 'da',text => '')`,
			  `scs_text.updateTextProc(text_id => ^profile_tid,language => 'en',text => '')`]
		      val keywords_tid = Db.Handle.oneFieldDb db `select scs.new_obj_id from dual`
		      val _ = Db.Handle.execSpDb db  
			[`scs_text.updateTextProc(text_id => ^keywords_tid,language => 'da',text => '')`,
			  `scs_text.updateTextProc(text_id => ^keywords_tid,language => 'en',text => '')`]
		      val empty_profile =
			{party_id = user_id,
			 profile_tid = (ScsError.valOf o Int.fromString) profile_tid,
			 profile_da = "",
			 profile_en = "",
			 keywords_tid = (ScsError.valOf o Int.fromString) keywords_tid,
			 keywords_da = "",
			 keywords_en = "",
			 edit_no = 0,
			 last_modified = ScsDate.now_local(),
			 modifying_user = 0,
			 deleted_p = false}
		      val ins_sql = `insert into scs_profiles 
			(party_id,profile_tid,keywords_tid,edit_no,last_modified,modifying_user)
			values
			('^(Int.toString user_id)','^(profile_tid)','^(keywords_tid)',
			 '0',sysdate,'^(Int.toString ScsLogin.default_id)')` 
		      val _ =Db.Handle.dmlDb db ins_sql
		    in
		      SOME empty_profile
		    end
		in
		  Db.Handle.dmlTrans new
		  handle _ => (Ns.log(Ns.Warning, "Could not create profile for user_id " ^ 
				      Int.toString user_id);
			       NONE)
		end
	  end
      fun getProfileErr (user_id,errs) =
	case getProfile user_id of
	  NONE => 
	    let
	      val err_msg = [(ScsLang.da,`Personen du søger findes ikke.`),
			     (ScsLang.en,`The person you are seeking does not exits.`)]
	    in
	      (NONE,ScsFormVar.addErr(ScsDict.s' err_msg,errs))
	    end
	| p => (p,errs)

    end


    fun nameToHtml (name,email) = Quot.toString
      `<a href="mailto:^(email)">^(name)</a>`

(* OBSOLETE: 2003-11-12, knp
    fun search_form target_url hvs =
      ScsWidget.formBox "/scs/person/person_search.sml" 
        [("submit",ScsDict.s [(ScsLang.en,`Search`),(ScsLang.da,`Søg`)])]
        (Html.export_hiddens (("target_url",target_url)::hvs) ^^ 
          (ScsDict.s' [(ScsLang.en,`Search after all persons that matches the pattern you type in below. 
			            Several fields related to a person are searched 
                                    including name, security number and email.`),
		       (ScsLang.da,`Søg efter alle personer som matcher det mønster du indtaster nedenfor.
                                    Der søges i flere felter, bl.a. navn, cpr nummer og email.`)]) ^^ `<p>` ^^
       (ScsWidget.tableWithTwoCols[(ScsDict.s' [(ScsLang.en,`Find person:`),
						(ScsLang.da,`Find person`)],ScsWidget.intext 40 "pat")]))
*)


    local 
      val infos = [
        (ScsLang.da, `Søg efter alle personer som matcher det mønster du 
		      indtaster nedenfor. Der søges i flere felter, bl.a. 
		      navn, cpr nummer og email. Du kan bruge % som 
		      wildcard.`),
        (ScsLang.en, `Search after all persons that matches the pattern you 
		     type in below. Several fields related to a person are 
		     searched including name, security number and email. You 
		     can use % as a wildcard.`)]
      val titles = UcsDict.search_person_dict
    in
      fun searchWidget m rw fv value_opt =
	UcsWidget.twoColumns (titles, infos, m, [UcsWidget.layoutValue rw
	  (UcsWidget.valOfString value_opt)
	  (ScsWidget.intextMaxLenVal 40 200 (ScsString.valOf value_opt) fv)
	] )
    end


    fun search_form target_url hvs = 
      let
        fun formbox() = UcsWidget.FORMBOX
	  {action     = "/scs/person/person_search.sml", 
	   form_attr  = [],
	   buts       = [("submit",ScsDict.s [(ScsLang.en,`Search`),(ScsLang.da,`Søg`)] ,NONE)],
	   header     = Html.export_hiddens (("target_url",target_url)::hvs),
	   table_attr = [],
	   body       = [searchWidget false UcsWidget.WRITE "pat" NONE]}
      in
        UcsWidget.layoutComponentGrp (formbox())
      end

    fun splitCpr cpr = (String.substring (cpr,0,6),String.substring (cpr,6,4))

    fun ppCpr cpr =
      let 
	val (x,y) = splitCpr cpr
      in
	x ^ "-" ^ y
      end
    handle _ => cpr

    fun makeCprPublic cpr =
      let
	val (cpr1,_) = splitCpr cpr
      in
	cpr1 ^ "-xxxx"
      end

    fun cprToDate cpr =
      let
	val (cpr1,_) = splitCpr cpr
	val day = Option.valOf(Int.fromString(String.substring(cpr1,0,2)))
	val mth = Option.valOf(Int.fromString(String.substring(cpr1,2,2)))
	val year = Option.valOf(Int.fromString(String.substring(cpr1,4,2)))
	val year = if year < 20 then 2000 + year else 1900 + year
      in
	ScsDate.genDate(day,mth,year)
      end

    fun cprToSex cpr =
      case String.substring(cpr,9,1) of
	"1" => Male
      | "3" => Male
      | "5" => Male
      | "7" => Male
      | "9" => Male
      | _ => Female

    (* do the following conversions:
         - if email is of form login@itu.dk => login@it-c.dk
         - if email is of form login@it.edu => login@it-c.dk
         - if email is of form login => login@it-c.dk
     *)
    fun fix_email email =
      let
	val email = ScsString.lower email
	val regExpExtract = RegExp.extract o RegExp.fromString
      in
	case regExpExtract "([a-z][a-z0-9\\-]*)@(it-c.dk|it.edu)" email of
	  SOME [l,e] => l ^ "@itu.dk"
	| _ => 
	    (case regExpExtract "([a-z][a-z0-9\\-]*)" email of
	       SOME [l] => l ^ "@itu.dk"
	     | _ => email)
      end
    (* Test code for fix_email
       fun try s =
         print (s ^ " = " ^ (fix_email s) ^ "\n")
       val _ =
         (try "nh";
          try "hanne@ruc.dk";
          try "nh@it-c.dk";
          try "nh@itu.dk";
          try "nh@it.edu";
          try "nh@diku.dk")
       handle Fail s => print s*)

    fun isFemale_p cpr = 
      cprToSex cpr = Female

    fun portrait_adm_nb (name_opt:string option) = 
      case name_opt of
        SOME name => (``, ScsDict.s' UcsDict.portrait_adm_dict ^^ ` for ` ^^
		      `^name`)
      | NONE	 => (``, ScsDict.s' UcsDict.portrait_adm_dict)

    val portrait_adm_help =

      `<h2>Status</h2>

      <b>todo:</b>Vi har styr på håndtering af billeder for ansatte
      samt synlighed for både ansatte og studerende. Vi mangler stadig
      at finde en fornuftig løsning for studerende<p>

      <h2>Portrætadministration</h2>

      Der findes både et <i>officielt</i> og et <i>ikke officielt</i>
      billede. Det officielle billede tages ved studiestart eller ved
      ansættelse og administreres af ansatte på ITU, der har fået
      tildelt rollen <b>PortraitAdm</b> i ^(ScsDict.getString MitItu.Ressources.mitITU_dict ScsLang.da). <p>

      Det ikke officielle billede uploades af brugerne selv.<p>

      Udover registrering af to billeder registreres <i>synlighed</i>
      af billedet, dvs. hvorvidt en bruger ønsker at billedet skal
      vises på Internettet eller om det skal være skjult.<p>

      Det officielle billede gemmes i henholdsvis den størrelse og
      kvalitet, som det der uploades, samt en thumbnail. Den oprindelige
      størrelse og kvalitet bevares, således at administrationen selv
      bestemmer kvaliteten. Nogle af de officielle billeder anvendes
      f.eks. i aviser og andre steder, hvor der kræves en vis
      kvalitet.<p>

      De ikke officielle billeder gemmes i en større udgave på højst
      400 pixels i højden og i en thumbnail. Dette sikrer et
      kontrolleret forbrug af diskplads.<p>

      <h3>Arbejdsgang</h3>

      Følgende opgaver varetages i forbindelse med administration af
      billeder:

      <ul>

      <li>Studieadministrationen registrerer synlighed for nye
      studerende. Nye studerende skriver under på hvorvidt deres
      billede må vises offentligt ved accept af studieplads eller
      tilmelding til enkeltfag.<p>

      <li>Personaleafdelingen registrerer synlighed for nye
      ansatte. Ansatte skriver under på hvorvidt deres billede må
      vises offentligt i forbindelse med deres ansættelseskontrakt.

      <li>Intern Service vedligeholder de officielle billeder for
      ansatte.

      <li><b>todo:</b> Det udestår stadig hvorvidt de studerenes
      billeder skal skabes i ^(ScsDict.getString MitItu.Ressources.mitITU_dict ScsLang.da) - måske de selv skal uploade dem.

      <li>Brugerne uploader selv de ikke officielle billeder.

      </ul>

      <h3>Administrationssiden</h3>

      Brugere med rollen <b>PortraitAdm</b> kan tilgå
      administrationssiden via deres egen hovedside. Linket
      "Portrætadministration" til siden findes i kassen "Site
      administration".<p>

      Når administrationssiden åbnes første gang vil der blot være en
      "søge-boks" hvori man kan søge efter den bruger hvis synlighed
      eller billeder skal redigeres:<p>

      <img src="/scs/person/images/search.png"><p>
      
      <h3>Synlighed</h3>

      På administrationssiden registreres synlighed i kassen med
      overskriften "Synlighed":<p>

      <img src="/scs/person/images/synlighed.png"><p>

      Vælges "Ja" betyder det at billederne må vises offentligt. Væges
      "Nej" betyder det at billederne kun vises til brugere med
      administrationsrettigheder, f.eks. studieadministration.<p>

      Ved at klikke på info-knappen til højre for "Gem" ses hvilke
      registreringer der er sket på dette felt.<p>

      Der findes pt. to definitioner af synlighed på ITU:

      <ol>

      <li>I ^(ScsDict.getString MitItu.Ressources.mitITU_dict ScsLang.da) 
	er synlighed defineret ved, at hvis der er angivet et
      "nej", så vises billedet hverken på Internettet eller
      Intranettet.

      <li>I billedarkivet i Intern Service vises alle billeder for
      både studerende og ansatte på Intranettet og kun på Internettet
      hvis der er svaret "ja" til synlighed.

      </ol>

      <b>TO-DO</b> Vi må finde ud af hvad der skal gælde!

      <h3>Gøre ikke officielt billede officielt</h3>

      Med knappen "Gør ikke officielt billede officielt" kan
      <b>PortraitAdm</b> kopiere det ikke officielle billede således
      at det også bliver det officielle:<p> 

      <img src="/scs/person/images/non_off_to_off_picture.png"><p>

      Dette er f.eks. nyttigt, hvis
      en bruger selv uploader et billede som derefter skal benyttes på
      adgangskort eller i en anden officielt forbindelse.<p>

      <h3>Visning af et billede</h3>

      Når et billede vises anvendes følgende prioriteter:
      
      <ol>
      <li> Hvis det ikke officielle billede findes, så vises det.
      <li> Hvis det officielle billede findes, så vises det.
      <li> Ellers vises et "tomt" billede med korrektur af en person
      </ol>

      <h3>Link til billede fra ekstern side</h3>

      Det er muligt at linke til billeder fra eksterne sider. Et link
      består af typen af billeder man ønsker (enten et thumbnail eller
      det store billede) samt en unik identifikation på personen. Der
      er pt. implementeret en unik identifikation, nemlig et internt
      løbenummer <code>person_id</code>. Men det vil også være muligt
      at implmentere andre unikke identifikatorer, f.eks. login eller
      email.<p>

      Du kan aflæse linket i boksen med titlen "Ekstern link til
      billede":<p>

      <img src="/scs/person/images/ext_link.png"><p>

      Linket sikrer, at et billede kun vises, hvis det er tilladt,
      dvs. kombinationen af login som kigger på billedet og billedets
      ejers synlighed checkes. Derudover vælges et billede efter
      reglerne ovenfor (Visning af et billede).

      <h3>Adgangskort</h3>
  
      Adgangskort laves i SysAdm via en ACCESS database. <b>Todo:</b>
      vi skal her undersøge om det er muligt at hente en url ind som
      billede eller om det kan hentes fra en hjemmeside?.

      `
  end
