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

    (* [getOrCreateFolderId (db,root_label,folder)] returns folder_id
       on folder named folder in the file storage instance named
       root_label. If no folder exists, then a row in the table
       scs_fs_folders is created. However, no physical folder is
       created - this is post poned to when an actual file or
       sub-folder is created. *)
    val getOrCreateFolderId : Db.Handle.db * root_label * string -> folder_id

    (* [uploadFolderForm
        (folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,fv_desc)]
        returns an upload form for adding, reading and deleting files
        in the folder represented by folder_id. The first result
        element is the number of files in the folder. *)
    val uploadFolderForm : 
      folder_id * string * priv * (string * string) list * string * string * string * string -> (int * quot)

    (* Modes for managing files *)
    val upload_mode_add    : string
    val upload_mode_delete : string
  end

structure ScsFileStorage =
  struct
    exception ScsFileStorage of string

    type root_label = string
    type folder_id = int

    datatype priv = 
      read            (* may read files *)
    | read_add        (* may read and add files *)
    | read_add_delete (* may read, add and delete files *)
    | admin           (* same as read_add_delete except that there are no restrictions on filesize etc. *)

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

    fun uploadFolderForm (folder_id,action,priv,hidden_fvs,fv_mode,fv_filename,fv_desc) = 
      let
	val (pre_header,pre_footer) =
	  if ScsList.contains priv [read_add,read_add_delete,admin] then
	    (`<form enctype=multipart/form-data method=post action="^(action)"> ` ^^
	     (Html.export_hiddens ((fv_mode,upload_mode_add) :: hidden_fvs)),
	     `<tr><td colspan="5">&nbsp;</td></tr>
	      <tr><td><input type=file size="20" name="^(fv_filename)"></td>
	      <td><input type=text size="20" name="^(fv_desc)"></td>
 	      <td>^(UcsPage.mk_submit("submit",ScsDict.s [(ScsLang.da,`Gem fil`),
							  (ScsLang.en,`Upload file`)],NONE))</td>
	      <td colspan="2">&nbsp;</td>
	      </tr></form>`)
	  else
	    (``,``)
	fun upload_fn_row bgcolor v = ``
	val files = []
      in
	(List.length files,
	 UcsPage.lineTable {hdcolor = "white",
			    width = "100%",
			    header = pre_header ^^ `
			    <tr bgcolor="#999999">
			    <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_filename_dict)</td>
			    <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_desc_dict)</td>
			    <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_filesize_dict)</td>
			    <td class="headercell">^(ScsDict.s UcsDict.scs_file_storage_upload_date_dict)</td>
			    <td class="headercell">^(ScsDict.s UcsDict.del_dict)</td>
			    </tr>`,
			    row_col1 = "#FFFFFF", 
			    row_col2 = "#EEEEEE",
			    align = "",
			    footer = pre_footer} upload_fn_row files)
      end

(* hertil 2003-06-18, nh
    fun uploadFile (user_id, priv, filename, description, filename_on_disk, filesize) =
      let
	new_rev_id <= max_rev_id : delete revision before you may upload new. If max rev is one, then delete file before you upload a new

        priv=add and filesize <= max_filesize orelse priv=admin

        priv=add and mimetype allowed orelse priv = admin

        priv=add and num_files <= max_files  orelse priv=admin
      in
	()
      end
*)

  end