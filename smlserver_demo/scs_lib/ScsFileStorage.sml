signature SCS_FILE_STORAGE = 
  sig
    (* The File Storage system is case sensitive! *)
    exception ScsFileStorage of string

    type root_label = string
    type folder_id = int

    datatype priv = 
      read            (* may read files *)
    | read_add        (* may read and add files *)
    | read_add_delete (* may read, add and delete files *)
    | admin           (* same as read_add_delete except that there 
                         are no restrictions on filesize etc. *)

    (* [mayDelFile_p priv] returns true if priv gives the privileges
        to delete a file *)
    val mayDelFile_p : priv -> bool

    (* [getOrCreateFolderId (db,root_label,folder)] returns folder_id
       on folder named folder in the file storage instance named
       root_label. If no folder exists, then a row in the table
       scs_fs_folders is created. However, no physical folder is
       created - this is post poned to when an actual file or
       sub-folder is created. *)
    val getOrCreateFolderId : Db.Handle.db * root_label * string -> folder_id

    type mime_type = {mime_type_id        : int,
		      mime_type           : string,
		      file_ext            : string,
		      file_icon           : string}

    type fs_type = {type_id             : int,
		    name                : string,
		    max_files           : int,
		    max_revisions       : int,
		    max_filesize_bytes  : int,
		    mime_types          : mime_type list} 

    (* [getFsType root_label] Return the file storage type for the
        instance represented by root_label. Returns NONE if no
        instance exists.*)
    val getFsType : Db.Handle.db * root_label -> fs_type option

    (* [uploadFolderForm
        (folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,fv_desc,fn_return_file,fn_del_file)]
        returns an upload form for adding, reading and deleting files
        in the folder represented by folder_id. The first result
        element is the number of files in the folder. The function
        fn_return_file takes a file_id and filename as argument and
        builds the url used to return the file. The function
        fn_del_file takes a file_id as argument and generates the url
        to call when deleting the file. *)
    val uploadFolderForm : 
      folder_id * string * priv * (string * string) list * string * string * string * 
      (int -> string -> string) * (int -> string) -> (int * quot)

    (* [uploadFile (db,user_id,folder_id,priv,fv_file,description)]
        uploads file represented by form variable fv_file with the
        given description. The file is uploaded to folder folder_id by
        user_id. Privileges are checked (priv) *)
    val uploadFile : Db.Handle.db * int * int * priv * string * string -> ScsFormVar.errs

    (* [returnFile file_id] returns the file file_id if exists;
        otherwise returns an error page to the user. *)
    val returnFile : int -> unit

    (* Modes for managing files *)
    val upload_mode_add    : string
    val upload_mode_delete : string

    (* [getFileIdErr (fv,errs)] checks that fv is an integer (i.e., a
        file_id) *)
    val getFileIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [delFile db (priv,file_id)] deletes the file file_id physically
        from the file storage, that is, removes the file from disk and
        delete rows in database *)
    val delFile : Db.Handle.db -> priv * int -> unit

   (* [numFilesInFolder folder_id] returns the number of files in
       folder - not counting subdirectories *)
    val numFilesInFolder : folder_id -> int
  end

structure ScsFileStorage :> SCS_FILE_STORAGE =
  struct
    exception ScsFileStorage of string

    type root_label = string
    type folder_id = int

    datatype priv = 
      read            (* may read files *)
    | read_add        (* may read and add files *)
    | read_add_delete (* may read, add and delete files *)
    | admin           (* same as read_add_delete except that there are no restrictions on filesize etc. *)

    fun mayDelFile_p priv =
      priv = read_add_delete orelse priv = admin

    (* Modes for managing files *)
    val upload_mode_add    = "add"
    val upload_mode_delete = "delete"

    fun getInstanceId (db,root_label) =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
	  `select scs_file_storage.getInstanceId(^(Db.qqq root_label))
             from dual` of
	NONE => NONE
      | SOME id => Int.fromString id

    fun getInstanceIdByFolderId (db,folder_id) =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
	  `select scs_file_storage.getInstanceIdByFolderId('^(Int.toString folder_id)')
             from dual` of
	     NONE => NONE
	   | SOME id => Int.fromString id

    fun getRootLabelByFolderId (db,folder_id) =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
	  `select scs_file_storage.getRootLabelByFolderId('^(Int.toString folder_id)')
             from dual` of r => r

    fun getNumFilesInFolderId (db,folder_id) =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
	  `select scs_file_storage.getNumFilesInFolderId('^(Int.toString folder_id)')
             from dual` of 
	     NONE => NONE
	   | SOME n => Int.fromString n
	       
    fun getRootFolderId (db,root_label,folder_name) =
      case 
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
  	  `select scs_file_storage.getRootFolderId(^(Db.qqq root_label),^(Db.qqq folder_name))
             from dual` of
	     NONE => NONE
	   | SOME id => Int.fromString id

    fun getSubFolderId (db,parent_folder_id,folder_name) =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
  	  `select scs_file_storage.getSubFolderId('^(Int.toString parent_folder_id)',^(Db.qqq folder_name))
             from dual` of
	   NONE => NONE
	 | SOME id => Int.fromString id

    local
      fun folder_name_on_disk folder_id folder_name = 
	Int.toString folder_id ^ "-" ^ (ScsFile.encodeFileNameUnix folder_name)
    in
      fun getOrCreateRootFolderId(db,root_label,folder_name) =
	case getRootFolderId(db,root_label,folder_name) of
	  SOME id => id
	| NONE => 
	    let
	      val folder_id = ScsDb.newObjIdDb db
	      val instance_id =
		case getInstanceId (db,root_label) of
		  NONE => raise ScsFileStorage ("ScsFileStorage.getOrCreateRootFolderId: can't get instance " ^
						"id on root_label:" ^ root_label ^ ".")
		| SOME id => id
	    in
	      ScsError.wrapPanic
	      (Db.Handle.dmlDb db)
	        `insert into scs_fs_folders
                   (folder_id,instance_id,parent_id,label,foldername,last_modifying_user)
		 values
		   (^(Int.toString folder_id),
		    ^(Int.toString instance_id),
		    null,
		    ^(Db.qqq (folder_name_on_disk folder_id folder_name)),
  		    ^(Db.qqq folder_name),
		    ^(Int.toString (ScsLogin.user_id())))`;
	       folder_id
	    end

      fun getOrCreateSubFolderId(db,parent_folder_id,folder_name) =
	case getSubFolderId(db,parent_folder_id,folder_name) of
	  SOME id => id
	| NONE => 
	    let
	      val folder_id = ScsDb.newObjIdDb db
	      val instance_id =
		case getInstanceIdByFolderId (db,parent_folder_id) of
		  NONE => raise ScsFileStorage ("ScsFileStorage.getOrCreateSubFolderId: can't get instance id on " ^
						"parent folder id: " ^ (Int.toString parent_folder_id) ^ ".")
		| SOME id => id
	    in
	      ScsError.wrapPanic
	      (Db.Handle.dmlDb db)
	        `insert into scs_fs_folders
                   (folder_id,instance_id,parent_id,label,foldername,last_modifying_user)
                 values
		   (^(Int.toString folder_id),
		    ^(Int.toString instance_id),
		    ^(Int.toString parent_folder_id),
		    ^(Db.qqq (folder_name_on_disk folder_id folder_name)),
		    ^(Db.qqq folder_name),
		    ^(Int.toString (ScsLogin.user_id())))`;
  	      folder_id
	    end
    end

    fun getOrCreateFolderId (db,root_label,dir) =
      case #arcs(Path.fromString dir) of
	[] => raise ScsFileStorage ("ScsFileStorage.getFolderId: can't create folder " 
				    ^ dir ^ " in instance " ^ root_label ^ ".")
      | root::sub_arcs => 
	  let
	    val root_folder_id = getOrCreateRootFolderId(db,root_label,root)
	    val folder_id =
	      List.foldl (fn (arch,parent_id) =>
			  getOrCreateSubFolderId(db,parent_id,arch))
	      root_folder_id sub_arcs
	  in
	    folder_id
	  end

    type mime_type = {mime_type_id        : int,
		      mime_type           : string,
		      file_ext            : string,
		      file_icon           : string}

    type file_type = {file_id             : int,
		      folder_id           : int,
		      revision_id         : int,
		      filename            : string,
		      description         : string,
		      filename_on_disk    : string,
		      filesize            : int,
		      last_modified       : Date.date,
		      last_modifying_user : int,
		      mime_type           : mime_type}

    local
      fun f g = {file_id = (ScsError.valOf o Int.fromString) (g "file_id"),
		 folder_id = (ScsError.valOf o Int.fromString) (g "folder_id"),
		 revision_id = (ScsError.valOf o Int.fromString) (g "revision_id"),
		 filename = g "filename",
		 description = g "description",
		 filename_on_disk = g "filename_on_disk",
		 filesize = (ScsError.valOf o Int.fromString) (g "filesize"),
		 last_modified = (ScsError.valOf o Db.toDate) (g "last_modified"),
		 last_modifying_user = (ScsError.valOf o Int.fromString) (g "last_modifying_user"),
		 mime_type = {mime_type_id = (ScsError.valOf o Int.fromString) (g "mime_type_id"),
			      mime_type = g "mime_type",
			      file_ext = g "file_extension",
			      file_icon = g "file_icon"}}
      fun genSql wh =
	(* Get latest revision of each file *)
	`select scs_fs_revisions.revision_id,
	        scs_fs_files.file_id,
	        scs_fs_folders.folder_id,
                scs_fs_files.name as filename,
	        scs_fs_files.description,
  		scs_fs_revisions.filename as filename_on_disk,
		scs_fs_revisions.filesize,
		scs_fs_revisions.last_modified,
		scs_fs_revisions.last_modifying_user,
		scs_fs_files.mime_type_id,
                scs_fs_mime_types.mime_type,
                scs_fs_mime_types.file_icon,
		scs_fs_mime_types.file_extension
           from scs_fs_mime_types,
		scs_fs_folders,
		scs_fs_files,
		scs_fs_revisions
          where scs_fs_folders.folder_id = scs_fs_files.folder_id
            and scs_fs_files.file_id = scs_fs_revisions.file_id
            and scs_fs_revisions.revision_id = scs_file_storage.getMaxRevisionId(scs_fs_files.file_id)
            and scs_fs_mime_types.id = scs_fs_files.mime_type_id
            ` ^^ wh


    in
      fun getFilesInFolderId folder_id : file_type list =
	let
	  val sql = genSql ` and scs_fs_folders.folder_id = '^(Int.toString folder_id)'`
	in
	  ScsError.wrapPanic (Db.list f) sql
	end
      fun getFileByFileId file_id : file_type option =
	let
	  val sql = genSql ` and scs_fs_files.file_id = '^(Int.toString file_id)'`
	in
	  Db.zeroOrOneRow' f sql
	end
    end

    fun numFilesInFolder folder_id =
      (ScsError.valOf o Int.fromString)
      (ScsError.wrapPanic
       Db.oneField `select count(file_id) 
                      from scs_fs_files
                     where folder_id = '^(Int.toString folder_id)'
                       and deleted_p = 'f'`)
	
    type fs_type = {type_id             : int,
		    name                : string,
		    max_files           : int,
		    max_revisions       : int,
		    max_filesize_bytes  : int,
		    mime_types          : mime_type list}

    fun getFsType (db, root_label) =
      let
	val sql = `select type_id, name, max_files, max_revisions, max_filesize
                     from scs_fs_types, scs_fs_instances
                    where scs_fs_types.type_id = scs_fs_instances.fs_type_id
                      and scs_fs_instances.label = ^(Db.qqq root_label)`
        fun f g = {type_id = (ScsError.valOf o Int.fromString) (g "type_id"),
		   name = g "name",
		   max_files = (ScsError.valOf o Int.fromString) (g "max_files"),
		   max_revisions = (ScsError.valOf o Int.fromString) (g "max_revisions"),
		   max_filesize_bytes = 1024 * (ScsError.valOf o Int.fromString) (g "max_filesize"),
		   mime_types = 
		   Db.fold
		   (fn (g,acc) => {mime_type_id = (ScsError.valOf o Int.fromString) (g "id"),
				   mime_type= g "mime_type", 
				   file_ext = g "file_extension", 
				   file_icon = g "file_icon"} :: acc)
		   [] `select m.mime_type, m.file_extension, m.file_icon,m.id
		         from scs_fs_mime_types m, scs_fs_types, scs_fs_type_mime_type_map
                        where scs_fs_types.type_id = '^(g "type_id")'
                          and scs_fs_types.type_id = scs_fs_type_mime_type_map.fs_type_id
                          and scs_fs_type_mime_type_map.mime_type_id = m.id`}
      in
	ScsError.wrapOpt (Db.Handle.oneRowDb' db f) sql
      end

    fun mkFileIcon (mime_type:mime_type) =
      let
	val img =
	  if #file_icon mime_type <> "" then
	    #file_icon mime_type
	  else
	    "/ucs/images/icon_document.gif"
      in
	Quot.toString `<img width="16" height="16" border="0" src="^img">`
      end

    fun uploadFolderForm (folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,fv_desc,
			  fn_return_file,fn_del_file) = 
      let
	val upload_info_dict = [(ScsLang.en,`You must choose a file on your local machine 
                                 and type in a description for that file.`),
				(ScsLang.da,`Du skal vælge en fil på din lokale maskine og 
                                 indtaste en beskrivelse af filen.`)]
	fun upload_fn_row bgcolor ({file_id,folder_id,revision_id,filename,description,filename_on_disk,
				    filesize,last_modified,last_modifying_user,mime_type}:file_type) = 
	  let
	    val confirm_del_dict = [(ScsLang.en,`Delete the file ^filename?`),
				    (ScsLang.en,`Slet filen ^filename?`)]
	  in
	    `<tr bgcolor="^bgcolor">
	     <td>^(mkFileIcon mime_type) ^(fn_return_file file_id filename)</td>
	     <td>^description</td>
	     <td align="right">^(ScsFile.ppFilesize filesize)</td>
             <td>^(ScsDate.pp last_modified)</td>
	     <td>^(if mayDelFile_p priv then
		     Quot.toString 
		     `<a ^(UcsPage.confirmOnClick (ScsDict.s confirm_del_dict)) 
		      href="^(fn_del_file file_id)"> ^(UcsPage.addSymbol false) </a>`
		   else
		     "&nbsp;")</td>
             </tr>`
	  end
	val files = getFilesInFolderId folder_id
	val (pre_header,pre_footer) =
	  if ScsList.contains priv [read_add,read_add_delete,admin] then
	    (`<form enctype=multipart/form-data method=post action="^(action)"> ` ^^
	     (Html.export_hiddens ((fv_mode,upload_mode_add) :: hidden_fvs)),
	     `<tr><td colspan="5">&nbsp;</td></tr>
	      <tr><td><input type=file size="20" name="^(fv_filename)"></td>
	      <td><input type=text size="30" name="^(fv_desc)"></td>
 	      <td>^(UcsPage.mk_submit("submit",ScsDict.s [(ScsLang.da,`Gem fil`),
							  (ScsLang.en,`Upload file`)],NONE)) 
	     ^(UcsPage.info (ScsDict.s upload_info_dict))</td>
	      <td colspan="2">&nbsp;</td>
	      </tr></form>`)
	  else
	    (``,``)
	val table =
	  if priv = read andalso null files then
	    `` (* No need to show table *)
	  else
	    UcsPage.lineTable {hdcolor = "white",
			       width = "100%",
			       header = pre_header ^^ `
			       <tr bgcolor="#999999">
			       <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_filename_dict)</td>
			       <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_desc_dict)</td>
			       <td class="headercell" align="right">
			       ^(ScsDict.s UcsDict.scs_file_storage_filesize_dict)</td>
			       <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_upload_date_dict)</td>
			       <td class="headercell">^(if mayDelFile_p priv then
							  ScsDict.s UcsDict.del_dict
							else 
							  "&nbsp;")</td>
			       </tr>`,
			       row_col1 = "#FFFFFF", 
			       row_col2 = "#EEEEEE",
			       align = "",
			       footer = pre_footer} upload_fn_row files
      in
	(List.length files,table)
      end

    local
      val file_missing_dict = [(ScsLang.en,`No file has been specified`),
			       (ScsLang.da,`Der er ikke angivet en fil`)]
      val filesize_missing_dict = [(ScsLang.en,`Can't find filesize - maybe no file is specified`),
				   (ScsLang.da,`Kan ikke beregne størrelse på fil - måske er der ikke angivet en fil.`)]
    in
      fun getFilenameErr (fv,errs) =	
	ScsFormVar.getStringErr(fv,ScsDict.s file_missing_dict,errs)
      fun getFilesizeErr (fv,errs) =
	ScsFormVar.getIntErr(fv^".filesize",ScsDict.s filesize_missing_dict,errs)
    end

    fun getFileIdByFolderIdAndFilename (db,folder_id,filename) =
      case
      ScsError.wrapPanic
      (Db.Handle.zeroOrOneFieldDb db)
        `select scs_file_storage.getFileIdByFolderIdAndFilename('^(Int.toString folder_id)', ^(Db.qqq filename))
           from dual` of
	   NONE => NONE
         | SOME fi => Int.fromString fi

    fun getNumRevisions (db,file_id) =
      case 
      ScsError.wrapPanic
      (Db.Handle.zeroOrOneFieldDb db)
        `select scs_file_storage.getNumRevisions('^(Int.toString file_id)')
           from dual` of
           NONE => NONE
         | SOME rv => Int.fromString rv

    fun getMimeTypeErr (file_ext,errs) =
      case
	ScsError.wrapPanic
	Db.zeroOrOneField
	`select scs_fs_mime_types.id
           from scs_fs_mime_types
          where lower(scs_fs_mime_types.file_extension) = lower(^(Db.qqq file_ext))` of
	  NONE => (0,ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`The file extension <b>%0</b> is not supported`),
						    (ScsLang.da,`Filendelsen <b>%0</b> er ikke supporteret`)]
				       [file_ext], errs))
	| SOME id => (ScsError.valOf (Int.fromString id),errs)

    fun getPath (db,folder_id) = 
      let
	val arcs = 
	  ScsError.wrapPanic
	  (Db.Handle.listDb db (fn g => g "label"))
	  ` select label
	      from scs_fs_folders
           connect by folder_id = prior parent_id
             start with folder_id = '^(Int.toString folder_id)'`
      in
	String.concatWith "/" (List.rev arcs)
      end

    fun getAbsPath db folder_id =
      ScsConfig.scs_file_storage_root() ^ "/" ^ 
      ScsError.valOf (getRootLabelByFolderId(db,folder_id)) ^ "/" ^ 
      getPath (db,folder_id)

    fun mime_type_allowed_p ({mime_types,...}: fs_type, file_ext') =
      List.exists (fn {mime_type_id,mime_type,file_ext,file_icon} => 
		   ScsString.lower file_ext = ScsString.lower file_ext') mime_types

    fun mkPhysFile (file_id,rev_id,file_base,file_ext) =
      Int.toString file_id ^ "-" ^ 
      Int.toString rev_id ^ "-" ^ 
      ScsFile.encodeFileNameUnix file_base ^ "." ^ 
      file_ext

    fun uploadFile (db, user_id, folder_id, priv, fv_file, description) =
      let
	val errs = ScsFormVar.emptyErr
	val (filename,errs) = getFilenameErr(fv_file,errs)
	(* Only keep filename (i.e., throw away directory) *)
	val filename = Path.file filename
	val fileextension = 
	  case Path.ext filename of
	    NONE => ""
	  | SOME ext => ext
	val filebase = Path.base filename
	val (filesize,errs) = getFilesizeErr(fv_file,errs)
	val (mime_type_id,errs) = getMimeTypeErr (fileextension,errs)
      in
	if errs <> ScsFormVar.emptyErr then
	  errs
	else
	  (* Continue checking mimetype, filesize, priv etc. *)
	  let
	    val root_label = ScsError.valOf (getRootLabelByFolderId (db,folder_id))
	    val fs_type = ScsError.valOf (getFsType (db,root_label))
	    val errs =
	      if priv = read then
		ScsFormVar.addErr(ScsDict.s' [(ScsLang.en,`You do not have the privileges to add a 
					       file in this directory.`),
					      (ScsLang.da,`Du har ikke rettigheder til at tilføje en 
					       fil i dette katalog.`)],errs)
	      else if priv = read_add orelse priv = read_add_delete then
		(* check constants in fs_type *)
		(if filesize > #max_filesize_bytes fs_type then
		   ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`The file you are storing is too large. The maximum size
						   is %0 and your file has size %1`),
						  (ScsLang.da,`Filen du er ved at gemme er for stor. Den maksimale
						   filstørrelse er %0 men din fil har størrelse %1.`)]
				     [ScsFile.ppFilesize (#max_filesize_bytes fs_type), 
				      ScsFile.ppFilesize filesize],errs)
		 else if not (mime_type_allowed_p (fs_type,fileextension)) then
		   ScsFormVar.addErr (ScsDict.sl' [(ScsLang.en,`The file has extension %0, however, you may only store
                                                    files with extension: %1`),
						   (ScsLang.da,`Filen har endelsen %0, men du kan kun gemme filer med 
						    endelserne: %1`)]
				      ["<b>"^fileextension^"</b>",
				       "<b>" ^ 
				       (String.concatWith "</b>,<b>" (List.map #file_ext (#mime_types fs_type))) ^ 
					"</b>"], errs)
		 else errs)
	      else errs (* priv = admin may do anything *)
     	  in
	    if errs <> ScsFormVar.emptyErr then
	      errs 
	    else
	      (* Continue with storing the file *)
	      let
		val file_id_opt = getFileIdByFolderIdAndFilename (db,folder_id,filename)
		fun storeFile file_id =
		  let
		    val new_rev_id = ScsDb.newObjIdDb db
		    val phys_file_name = mkPhysFile (file_id,new_rev_id,filebase,fileextension)
		    val _ = Db.Handle.dmlDb db
		      `insert into scs_fs_revisions (revision_id, file_id, mime_type_id, 
						     filename, filesize, last_modifying_user)
		         values ('^(Int.toString new_rev_id)', '^(Int.toString file_id)', 
				 '^(Int.toString mime_type_id)', ^(Db.qqq phys_file_name), 
				 '^(Int.toString filesize)', '^(Int.toString (ScsLogin.user_id()))')`
		    val path = getAbsPath db folder_id
		    val _ = ScsError.wrapPanic ScsFile.mkDir path
		    val _ = ScsError.wrapPanic Ns.Conn.storeMultiformData (fv_file,path ^ "/" ^ phys_file_name)
		  in
		    ()
		  end
	      in
		case file_id_opt of
		  NONE => 
                    (* We are storing a new file *)
		    let
		      val num_files_in_folder = ScsError.valOf (getNumFilesInFolderId(db,folder_id))
		    in
		      if #max_files fs_type <= num_files_in_folder andalso priv <> admin then
			ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`You can't store file because there is already %0 
							files in the directory. You are only allowed to store %1
							files in the directory.`),
						       (ScsLang.da,`Du kan ikke gemme filen fordi der er allerede
							gemt %0 filer i kataloget. Du kan ikke gemme mere end %1 filer
							i kataloget.`)]
					  [Int.toString num_files_in_folder, Int.toString (#max_files fs_type)],errs)
		      else
			(* Store New File *)
			let
			  val new_file_id = ScsDb.newObjIdDb db
			  val _ = Db.Handle.dmlDb db 
			    `insert into scs_fs_files (file_id, folder_id, name, description, mime_type_id, last_modifying_user)
                               values ('^(Int.toString new_file_id)','^(Int.toString folder_id)', ^(Db.qqq filename),
				       ^(Db.qqq description), 
				       scs_file_storage.getMimeTypeIdByFileExt(lower(^(Db.qqq fileextension))),
				       '^(Int.toString (ScsLogin.user_id()))')`
			  val _ = storeFile new_file_id
			in
			  errs
			end
		    end
		  | SOME file_id =>
		    (* We are storing a new revision of an existing file *)
		    let
		      val num_revs = ScsError.valOf (getNumRevisions (db,file_id))
		    in
		      if #max_revisions fs_type <= num_revs andalso priv <> admin then
			(if num_revs = 1 then
			   ScsFormVar.addErr(ScsDict.s' [(ScsLang.en,`You can't store the file ^filename because the file already
							  exists.`),
							 (ScsLang.da,`Du kan ikke gemme filen ^filename fordi den allerede 
							  eksisterer.`)], errs)
			 else
			   ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`You can't store the file ^filename because 
							   there already exists
							   %0 revisions. You are only allowed to store %1 revisions.`),
							  (ScsLang.da,`Du kan ikke gemme filen ^filename fordi der allerede 
							   eksisterer %0 versioner. Du kan kun gemme %1 versioner af 
							   samme fil.`)]
					     [Int.toString num_revs, Int.toString (#max_revisions fs_type)],errs))
		      else
			(* Store New Revision *)
			let
			  val _ = Db.Handle.dmlDb db 
			    `update scs_fs_files 
                                set ^(Db.setList [("description",description),
						  ("last_modifying_user",Int.toString (ScsLogin.user_id()))]),
				    mime_type_id = scs_file_storage.getMimeTypeIdByFileExt(lower(^(Db.qqq fileextension))),
				    last_modified = sysdate
                             where file_id = '^(Int.toString file_id)'`
			  val _ = storeFile file_id
			in
			  errs
			end
		    end
	      end
	  end
      end

    fun returnFile file_id =
      case getFileByFileId file_id of
	NONE => (ScsPage.returnPg (ScsDict.s [(ScsLang.en,`File not found`),
					      (ScsLang.da,`Fil findes ikke`)])
		 (ScsDict.s' [(ScsLang.en,`The file does not exists - please try again or 
			       <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the administrator</a>.`),
			      (ScsLang.da,`Filen findes ikke - du kan prøve igen eller 
			       <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte administrator</a>.`)]);
		 ())
      | SOME file =>
	  let
	    val path = Db.Handle.wrapDb getAbsPath (#folder_id file)
	    val filename_on_disk = path ^ "/" ^ #filename_on_disk file
	    val _ = Ns.returnFile filename_on_disk
	  in
	   ()
	  end

    fun getFileIdErr(fv,errs) = 
      ScsFormVar.getNatErr(fv,ScsDict.s [(ScsLang.da,`Nummer på fil`),(ScsLang.en,`File number`)],errs)

    fun delFile db (priv,file_id) =
      if mayDelFile_p priv then
	case getFileByFileId file_id of
	  NONE => (ScsPage.returnPg (ScsDict.s [(ScsLang.en,`File not found`),
						(ScsLang.da,`Fil findes ikke`)])
		   (ScsDict.s' [(ScsLang.en,`The file does not exists - please try again or 
				 <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the administrator</a>.`),
				(ScsLang.da,`Filen findes ikke - du kan prøve igen eller 
				 <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte administrator</a>.`)]);
		   ())
	| SOME file =>
	    let
	      val path = Db.Handle.wrapDb getAbsPath (#folder_id file)
	      val files_to_delete =
		Db.Handle.listDb db (fn g => g "filename") `select filename 
                                                              from scs_fs_revisions
                                                             where file_id = '^(Int.toString file_id)'`
	      val _ = (* We delete all revisions *)
		Db.Handle.dmlDb db `delete from scs_fs_revisions
                                     where file_id = '^(Int.toString file_id)'`
	      val _ =
		Db.Handle.dmlDb db `delete from scs_fs_files
		                     where file_id = '^(Int.toString file_id)'`
	      val _ = List.app (fn filename_on_disk => 
				FileSys.remove (path ^ "/" ^ filename_on_disk)
				handle _ => ()) files_to_delete
	    in
	      ()
	    end
      else
	(ScsPage.returnPg (ScsDict.s [(ScsLang.en,`Can't delete file`),
				      (ScsLang.da,`Kan ikke slette fil`)])
	 (ScsDict.s' [(ScsLang.en,`You do not have the privileges to delete the file. If you believe this
		       is an error, then please 
		       <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the administrator</a>.`),
		      (ScsLang.da,`Du har ikke adgang til at slette filen. Hvis du mener dette er forkert, 
		       så kan du <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte administrator</a>.`)]);
	 ())
  end