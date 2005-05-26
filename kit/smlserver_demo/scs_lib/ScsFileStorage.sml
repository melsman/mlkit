
signature SCS_FILE_STORAGE = 
  sig
    (* The File Storage system is case sensitive! *)
    exception ScsFileStorage of string

    type root_label = string
    type folder_id = int

    datatype priv = 
      no_priv         (* may not read files *)
    | read            (* may read files *)
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

    type file_type = {file_id                   : int,
		      folder_id                 : int,
		      revision_id               : int,
		      filename                  : string,
		      description               : string,
		      filename_on_disk          : string,
		      path_on_disk              : string,
		      filesize                  : int,
		      last_modified             : Date.date,
		      last_modifying_user       : int,
		      mime_type                 : mime_type}


    (* [getFsType root_label] Return the file storage type for the
        instance represented by root_label. Returns NONE if no
        instance exists.*)
    val getFsType : Db.Handle.db * root_label -> fs_type option

    (* [uploadFolderForm
        (root_label,folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,
	 fv_desc,fn_return_file,fn_del_file,upload_info_dict)]
        returns an upload form for adding, reading and deleting files
        in the folder represented by folder_id. The first result
        element is the number of files in the folder. The function
        fn_return_file takes a file_id and filename as argument and
        builds the url used to return the file. The function
        fn_del_file takes a file_id as argument and generates the url
        to call when deleting the file. The argument upload_info_dict
	contains a help message placed at right of the upload-button.*)
    val uploadFolderForm : 
      root_label * folder_id * string * priv * (string * string) list * string * string * string * 
      (int -> string -> string -> string) * (int -> string) * ScsDict.dict -> (int * quot)

    (* [uploadFile (db,user_id,folder_id,priv,fv_file,description)]
        uploads file represented by form variable fv_file with the
        given description. The file is uploaded to folder folder_id by
        user_id. Privileges are checked (priv). A file_id and physical
        path and filename of uploaded file is returned. *)
    val uploadFile : Db.Handle.db * int * int * priv * string * string -> int * string * ScsFormVar.errs

    (* [storeFile
        (db,user_id,folder_id,priv,phys_filename,db_filename,description)]
        stores the file phys_filename with the given description. The
        file is named db_filename in the database. The file is stored
        in folder folder_id by user_id. Privileges are checked
        (priv). A new file_id and a complete path to the new physical
        file is returned. *)
    val storeFile : Db.Handle.db * int * int * priv * string * string * string -> int * string * ScsFormVar.errs

    (* [replaceFile (db,user_id,file:
       file_type,priv,source_file,errs)] replaces the file described
       by file with new source file. It's the latest revision that is
       replaced. Privileges are checked (priv). *)
    val replaceFile : Db.Handle.db * int * file_type * priv * string * ScsFormVar.errs -> ScsFormVar.errs

    (* [returnFile root_label file_id] returns the file file_id if
        exists in the given root_label; otherwise returns an error
        page to the user. The root_label increase security so that it
        is required that file_id is actually in the instance named
        root_label. *)
    val returnFile : root_label -> int -> unit

    (* Modes for managing files *)
    val upload_mode_add    : string
    val upload_mode_delete : string

    (* [getFileIdErr (fv,errs)] checks that fv is an integer (i.e., a
        file_id) *)
    val getFileIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [getFilenameErr (fv_filename,errs)] returns the filename
        represented by form variable fv_filename. *)
    val getFilenameErr : string * ScsFormVar.errs -> string * ScsFormVar.errs

    (* [getFilesizeErr (fv_filename,errs)] returns the file size
        represented by form variable fv_filename. *)
    val getFilesizeErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [delFile db root_label (priv,file_id)] deletes the file file_id physically
        from the file storage, that is, removes the file from disk and
        delete rows in database *)
    val delFile    : Db.Handle.db -> root_label -> priv * int -> unit

    (* [delFile db root_label (priv,file_id,errs)] same as delFile
        except that errors are returned in errs. *)
    val delFileErr : Db.Handle.db -> root_label -> priv * int * ScsFormVar.errs -> ScsFormVar.errs

    (* [getNumFilesInFolderId folder_id] returns the number of files in
        folder - not counting subdirectories *)
    val getNumFilesInFolderId : Db.Handle.db -> folder_id -> int

    (* [storeMultiformData (fv_filename,target_file,errs)] store an
        uploaded file fv_filename in target_file. If unsuccessful an
        error is appended to errs. *)
    val storeMultiformData : string * string * ScsFormVar.errs -> ScsFormVar.errs

    (* [getFilesInFolderId root_label folder_id] returns the files in
        folder folder_id stored in the fs-instance root_label. *)
    val getFilesInFolderId : string -> int -> file_type list

    (* [getFileByFileId root_label file_id] returns the file
        represented by file_id in fs-instance root_label. *)
    val getFileByFileId : string -> int -> file_type option
  end

structure ScsFileStorage :> SCS_FILE_STORAGE =
  struct
    exception ScsFileStorage of string

    type root_label = string
    type folder_id = int

    datatype priv = 
      no_priv         (* may not read files *)
    | read            (* may read files *)
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
	  `select scs_file_storage.getRootLabelByFolderId('^(Int.toString folder_id)') as root_label
             from dual` of r => r

    fun getNumFilesInFolderId db folder_id =
      case
	ScsError.wrapPanic
	(Db.Handle.zeroOrOneFieldDb db)
	  `select scs_file_storage.getNumFilesInFolderId('^(Int.toString folder_id)')
             from dual` of 
	     NONE => 0
	   | SOME n => (ScsError.valOf o Int.fromString) n
	       
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

    type file_type = {file_id                   : int,
		      folder_id                 : int,
		      revision_id               : int,
		      filename                  : string,
		      description               : string,
		      filename_on_disk          : string,
		      path_on_disk              : string,
		      filesize                  : int,
		      last_modified             : Date.date,
		      last_modifying_user       : int,
		      mime_type                 : mime_type}

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

    fun getAbsPath folder_id db =
      let
	val root = ScsConfig.scs_file_storage_root()
	val root_label = ScsError.valOf (getRootLabelByFolderId(db,folder_id))
	val path = getPath (db,folder_id)
      in
	root ^ "/" ^ root_label ^ "/" ^ path
      end
    local
      fun f g = 
	let
	  val folder_id = (ScsError.valOf o Int.fromString) (g "folder_id")
	in
	  {file_id = (ScsError.valOf o Int.fromString) (g "file_id"),
	   folder_id = folder_id,
	   revision_id = (ScsError.valOf o Int.fromString) (g "revision_id"),
	   filename = g "filename",
	   description = g "description",
	   filename_on_disk = g "filename_on_disk",
	   path_on_disk = 
	     ScsConfig.scs_file_storage_root() ^ "/" ^ 
	     (g "root_label") ^ "/" ^ (g "path_on_disk") ^ (g "label") ^ "/",
	   filesize = (ScsError.valOf o Int.fromString) (g "filesize"),
	   last_modified = (ScsError.valOf o Db.toDate) (g "last_modified"),
	   last_modifying_user = (ScsError.valOf o Int.fromString) (g "last_modifying_user"),
	   mime_type = {mime_type_id = (ScsError.valOf o Int.fromString) (g "mime_type_id"),
			mime_type = g "mime_type",
			file_ext = g "file_extension",
			file_icon = g "file_icon"}}
	end
      fun genSql root_label wh =
	(* Get latest revision of each file *)
	`select scs_fs_revisions.revision_id,
	        scs_fs_files.file_id,
	        scs_fs_folders.folder_id,
		scs_fs_folders.path_on_disk,
                scs_fs_folders.label,
                scs_fs_instances.label as root_label,
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
		scs_fs_revisions,
		scs_fs_instances
          where scs_fs_folders.folder_id = scs_fs_files.folder_id
            and scs_fs_folders.instance_id = scs_fs_instances.instance_id
            and scs_fs_instances.label = ^(Db.qqq root_label)
            and scs_fs_files.file_id = scs_fs_revisions.file_id
            and scs_fs_revisions.revision_id = scs_file_storage.getMaxRevisionId(scs_fs_files.file_id)
            and scs_fs_mime_types.id = scs_fs_files.mime_type_id
            ` ^^ wh
    in
      fun getFilesInFolderId root_label folder_id : file_type list =
	let
	  val sql = genSql root_label ` and scs_fs_folders.folder_id = '^(Int.toString folder_id)'`
	in
	  ScsError.wrapPanic (Db.list f) sql
	end
      fun getFileByFileId root_label file_id : file_type option =
	let
	  val sql = genSql root_label ` and scs_fs_files.file_id = '^(Int.toString file_id)'`
	in
	  Db.zeroOrOneRow' f sql
	end
    end
	
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

    fun uploadFolderForm (root_label,folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,fv_desc,
			  fn_return_file,fn_del_file,upload_info_dict) = 
      let
	fun upload_fn_row bgcolor ({file_id,folder_id,revision_id,filename,description,filename_on_disk,
				    path_on_disk,
				    filesize,last_modified,last_modifying_user,mime_type}:file_type) = 
	  let
	    val confirm_del_dict = [(ScsLang.en,`Delete the file ^filename?`),
				    (ScsLang.da,`Slet filen ^filename?`)]
	  in
	    `<tr bgcolor="^bgcolor">
	     <td>^(fn_return_file file_id filename (mkFileIcon mime_type ^ " " ^ filename))</td>
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
	val files = getFilesInFolderId root_label folder_id
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
	  if priv = no_priv orelse (priv = read andalso null files) then
	    `` (* No need to show table *)
	  else
	    UcsPage.lineTable {
	      hdcolor = "white",
	      width = "100%",
	      header = pre_header ^^ (`
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
		  </tr>`
	      ),
	      row_col1 = "#FFFFFF", 
	      row_col2 = "#EEEEEE",
	      align = "",
	      footer = pre_footer
	    } upload_fn_row files
      in
	(List.length files,table)
      end

    local
      val file_missing_dict = [(ScsLang.en,`No file has been specified`),
			       (ScsLang.da,`Der er ikke angivet en fil`)]
    in
      val filesize_missing_dict = [(ScsLang.en,`Can't find filesize - maybe no file is specified`),
				   (ScsLang.da,`Kan ikke beregne størrelse på fil - måske er der ikke angivet en fil.`)]
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

    (* getMimeTypeErr never fails - we just return */* mime type if
       file_ext is empty or unknown. *)
    fun getMimeTypeErr (file_ext,errs) =
      case
	ScsError.wrapPanic
	Db.zeroOrOneField
	`select scs_fs_mime_types.id
           from scs_fs_mime_types
          where lower(scs_fs_mime_types.file_extension) = lower(^(Db.qqq file_ext))` of
	  NONE => (ScsError.wrapPanic
	          (ScsError.valOf o Int.fromString o Db.oneField) 
		  `select scs_fs_mime_types.id
                     from scs_fs_mime_types
                    where scs_fs_mime_types.mime_type = '*/*'`,errs)
	| SOME id => (ScsError.valOf (Int.fromString id),errs)

    fun mime_type_allowed_p ({mime_types,...}: fs_type, file_ext') =
      List.exists (fn {mime_type_id,mime_type,file_ext,file_icon} => 
		   ScsString.lower file_ext = ScsString.lower file_ext') mime_types

    fun mkPhysFile (file_id,rev_id,file_base,file_ext) =
      Int.toString file_id ^ "-" ^ 
      Int.toString rev_id ^ "-" ^ 
      ScsFile.encodeFileNameUnix file_base ^ "." ^ 
      file_ext
      
    local
      fun check_of_file_1 (filename,errs) =
	let
	  (* Only keep filename (i.e., throw away directory) *)
	  val filename = (ScsPathMac.file o ScsPathWin.file o ScsPathUnix.file) filename
	  val fileextension = 
	    case Path.ext filename of
	      NONE => ""
	    | SOME ext => ext
	  val filebase = Path.base filename
	  val (mime_type_id,errs) = getMimeTypeErr (fileextension,errs)
	in
	  (filename,fileextension,filebase,mime_type_id,errs)
	end

      fun check_of_file_2 (db,fileextension,filesize,priv,folder_id,errs) =
	(* Check mimetype, filesize, priv etc. *)
	let
	  val root_label = ScsError.valOf (getRootLabelByFolderId (db,folder_id))
	  val fs_type = ScsError.valOf (getFsType (db,root_label))
	  val errs =
	    if priv = read orelse priv = no_priv then
	      ScsFormVar.addErr(ScsDict.s' [(ScsLang.en,`You do not have the privileges to add a 
					     file in this directory.`),
					    (ScsLang.da,`Du har ikke rettigheder til at tilføje en 
					     fil i dette katalog.`)],errs)
	    else if priv = read_add orelse priv = read_add_delete then
	      (* check constants in fs_type *)
	      (if filesize > #max_filesize_bytes fs_type then
		 ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`The file you are storing is too large. The 
						 maximum size is %0 and your file has size %1`),
						(ScsLang.da,`Filen du er ved at gemme er for stor. Den maksimale
						 filstørrelse er %0 men din fil har størrelse %1.`)]
				   [ScsFile.ppFilesize (#max_filesize_bytes fs_type), 
				    ScsFile.ppFilesize filesize],errs)
	       else if not (mime_type_allowed_p (fs_type,fileextension)) then
		 ScsFormVar.addErr (ScsDict.sl' [(ScsLang.en,`The filetype %0 is not supported. You can upload 
						  files of type %1.`),
						 (ScsLang.da,`Filtypen %0 er ikke understøttet. Du kan kun gemme 
						  filer af typen: %1.`)]
				    ["<b>"^fileextension^"</b>",
				     "<b>" ^ 
				     (String.concatWith "</b>,<b> " (List.map #file_ext (#mime_types fs_type))) ^ 
				     "</b>"], errs)
		    else errs)
		 else errs (* priv = admin may do anything *)
        in
          (fs_type,errs)
	end

      fun uploadFile' (filename,filesize,saveFile: string -> unit) 
	(db, user_id, folder_id, priv, description, errs) =
	let
	  val (filename,fileextension,filebase,mime_type_id,errs) = check_of_file_1 (filename,errs)
	in
	  if errs <> ScsFormVar.emptyErr then
	    (0,"",errs)
	  else
	    (* Continue checking mimetype, filesize, priv etc. *)
	    let
	      val (fs_type,errs) = check_of_file_2(db,fileextension,filesize,priv,folder_id,errs)
            in
	      if errs <> ScsFormVar.emptyErr then
		(0,"",errs)
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
		      val path = getAbsPath folder_id db 
		      val _ = ScsError.wrapPanic ScsFile.mkDir path
		      val phys_path_and_file_name = path ^ "/" ^ phys_file_name
		      val _ = saveFile phys_path_and_file_name
		    in
		      (file_id,phys_path_and_file_name)
		    end
		in
		  case file_id_opt of
		    NONE => 
		      (* We are storing a new file *)
		      let
			val num_files_in_folder = getNumFilesInFolderId db folder_id
		      in
			if #max_files fs_type <= num_files_in_folder andalso priv <> admin then
			  (0,"",ScsFormVar.addErr(ScsDict.sl' 
						  [(ScsLang.en,`You can't store file because there is 
						    already %0 files in the directory. You are only allowed 
						    to store %1 files in the directory.`),
						   (ScsLang.da,`Du kan ikke gemme filen fordi der er allerede
						    gemt %0 filer i kataloget. Du kan ikke gemme mere end %1 
						    filer i kataloget.`)]
						  [Int.toString num_files_in_folder, 
						   Int.toString (#max_files fs_type)],
						  errs))
			else
			  (* Store New File *)
			  let
			    val new_file_id = ScsDb.newObjIdDb db
			    val _ = Db.Handle.dmlDb db 
			      `insert into scs_fs_files (file_id, folder_id, name, description, 
							 mime_type_id, last_modifying_user)
			       values ('^(Int.toString new_file_id)','^(Int.toString folder_id)', ^(Db.qqq filename),
				       ^(Db.qqq description), 
				       scs_file_storage.getMimeTypeIdByFileExt(lower(^(Db.qqq fileextension))),
				       '^(Int.toString (ScsLogin.user_id()))')`
			    val (file_id,phys_path_and_file_name) = storeFile new_file_id
			  in
			    (file_id,phys_path_and_file_name,errs)
			  end
		      end
	          | SOME file_id =>
		    (* We are storing a new revision of an existing file *)
		    let
		      val num_revs = ScsError.valOf (getNumRevisions (db,file_id))
		    in
		      if #max_revisions fs_type <= num_revs andalso priv <> admin then
			(if num_revs = 1 then
			   (0,"",ScsFormVar.addErr(ScsDict.s' [(ScsLang.en,`You can't store the file ^filename 
								because the file already exists.`),
							       (ScsLang.da,`Du kan ikke gemme filen ^filename 
								fordi den allerede eksisterer.`)], errs))
			 else
			   (0,"",ScsFormVar.addErr(ScsDict.sl' [(ScsLang.en,`You can't store the file ^filename 
								 because there already exists
								 %0 revisions. You are only allowed to
								 store %1 revisions.`),
								(ScsLang.da,`Du kan ikke gemme filen ^filename fordi 
								 der allerede eksisterer %0 versioner. Du kan kun 
								 gemme %1 versioner af samme fil.`)]
						   [Int.toString num_revs, Int.toString (#max_revisions fs_type)],
						   errs)))
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
			  val (file_id,phys_path_and_file_name) = storeFile file_id
			in
			  (file_id,phys_path_and_file_name,errs)
			end
		    end
	    end
	  end
      end
    in
      fun uploadFile (db, user_id, folder_id, priv, fv_file, description) =
	let
	  val errs = ScsFormVar.emptyErr
	  val (filename,errs) = getFilenameErr(fv_file,errs)
	  val (filesize,errs) = getFilesizeErr(fv_file,errs)
	  fun saveFile phys_filename =
	    ScsError.wrapPanic Ns.Conn.storeMultiformData (fv_file,phys_filename)
	in
	  uploadFile' (filename,filesize,saveFile) (db, user_id, folder_id, priv, description, errs)
	end
      fun storeFile (db,user_id,folder_id,priv,phys_filename,db_filename,description) =
	let
	  val errs = ScsFormVar.emptyErr
	  val (filesize,errs) = 
	    (FileSys.fileSize phys_filename,errs)
	    handle _ => (0,ScsFormVar.addErr(ScsDict.s' filesize_missing_dict,errs))
	  fun saveFile fs_filename = ScsError.wrapPanic (ScsFile.cp phys_filename) fs_filename
	in
	  uploadFile' (db_filename,filesize,saveFile) (db, user_id, folder_id, priv, description, errs)
	end	  
      fun replaceFile (db,user_id,file: file_type,priv,source_file,errs) =
	let
	  val (filename,fileextension,filebase,mime_type_id,errs) = check_of_file_1 (source_file,errs)
	in
	  if errs <> ScsFormVar.emptyErr then errs 
	  else
	    let
	      val (filesize,errs) = (FileSys.fileSize source_file,errs)
		handle _ => (0,ScsFormVar.addErr(ScsDict.s' filesize_missing_dict,errs))
	      val (fs_type,errs) = check_of_file_2(db,fileextension,filesize,priv,#folder_id file,errs)
	    in
	      if errs <> ScsFormVar.emptyErr then errs
	      else
		(* Now replace file on disk and update DB with new size and name. *)
		let
		  val _ = ScsError.wrapPanic (Db.Handle.dmlDb db)
		    `update scs_fs_files
                        set ^(Db.setList [("last_modifying_user",Int.toString (ScsLogin.user_id()))]),
			    mime_type_id = scs_file_storage.getMimeTypeIdByFileExt(lower(^(Db.qqq fileextension))),
			    last_modified = sysdate
                           where file_id = '^(Int.toString (#file_id file))'`
		  val _ = ScsError.wrapPanic (Db.Handle.dmlDb db)
		    `update scs_fs_revisions
                       set ^(Db.setList [("filesize",Int.toString filesize),
					 ("last_modifying_user",Int.toString (ScsLogin.user_id()))]),
			    mime_type_id = scs_file_storage.getMimeTypeIdByFileExt(lower(^(Db.qqq fileextension))),
			    last_modified = sysdate
                      where revision_id = '^(Int.toString (#revision_id file))'`
		  val _ = ScsError.wrapPanic (ScsFile.cp source_file) 
		    (#path_on_disk file ^ (#filename_on_disk file))
		in
		  errs
		end
	    end
	end
    end

    fun returnFile root_label file_id =
      case getFileByFileId root_label file_id of
	NONE => (ScsPage.returnPg (ScsDict.s UcsDict.file_not_found_dict)
		 (ScsDict.s' [(ScsLang.en,`The file does not exists - please try again or 
			       <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the administrator</a>.`),
			      (ScsLang.da,`Filen findes ikke - du kan prøve igen eller 
			       <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte administrator</a>.`)]);
		 ())
      | SOME file =>
	  let
	    val path = #path_on_disk file 
	    val filename_on_disk = path ^ #filename_on_disk file
	    fun return_error () = 
	      (ScsPage.returnPg 
	       (ScsDict.s UcsDict.file_not_found_dict) 
	       (ScsDict.s' [(ScsLang.en,`The file does not exists - the administrator has been
			     notified about the problem.`),
			    (ScsLang.da,`Filen findes ikke - administrator er blever informeret om problemet.`)]);
	       ScsError.emailError `The file ^filename_on_disk (file_id = ^(Int.toString file_id)) exists in 
	       database but not on the disk???`)
          in
	   ((if FileSys.fileSize filename_on_disk > 0 then
	       (Ns.returnFile filename_on_disk;())
	     else 
	       return_error())
	    handle _ => return_error();
	      ())
	  end

    fun getFileIdErr(fv,errs) = 
      ScsFormVar.getNatErr(fv,ScsDict.s [(ScsLang.da,`Nummer på fil`),(ScsLang.en,`File number`)],errs)

    fun delFile db root_label (priv,file_id) =
      if mayDelFile_p priv then
	case getFileByFileId root_label file_id of
	  NONE => (ScsPage.returnPg (ScsDict.s [(ScsLang.en,`File not found`),
						(ScsLang.da,`Fil findes ikke`)])
		   (ScsDict.s' [(ScsLang.en,`The file does not exists - please try again or 
				 <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the administrator</a>.`),
				(ScsLang.da,`Filen findes ikke - du kan prøve igen eller 
				 <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte administrator</a>.`)]);
		   ())
	| SOME file =>
	    let
	      val path = #path_on_disk file
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
				FileSys.remove (path ^ filename_on_disk)
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

    fun delFileErr db root_label (priv,file_id,errs) =
      if mayDelFile_p priv then
	case getFileByFileId root_label file_id of
	  NONE => ScsFormVar.addErr (ScsDict.s' 
				     [(ScsLang.en,`The file does not exists - please try again or 
				       <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact the 
				       administrator</a>.`),
				      (ScsLang.da,`Filen findes ikke - du kan prøve igen eller 
				       <a href="mailto:^(ScsConfig.scs_site_adm_email())">kontakte 
				       administrator</a>.`)],errs)
	| SOME file =>
	    let
	      val path = #path_on_disk file
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
				FileSys.remove (path ^ filename_on_disk)
				handle _ => ()) files_to_delete
	    in
	      errs
	    end
      else
	ScsFormVar.addErr(ScsDict.s' [(ScsLang.en,`You do not have the privileges to delete the file. If 
				       you believe this is an error, then please 
				       <a href="mailto:^(ScsConfig.scs_site_adm_email())">contact 
				       the administrator</a>.`),
				      (ScsLang.da,`Du har ikke adgang til at slette filen. Hvis du mener dette er 
				       forkert, så kan du <a href="mailto:^(ScsConfig.scs_site_adm_email())">
				       kontakte administrator</a>.`)],errs)

    fun storeMultiformData (fv_filename,target_filename,errs) =
      (Ns.Conn.storeMultiformData(fv_filename,target_filename);
       errs)
      handle _ => ScsFormVar.addErr(ScsDict.s' [(ScsLang.da,`Der er opstået en fejl ved upload af fil.`),
						(ScsLang.en,`An error happended when uploading file.`)],errs)
  end
