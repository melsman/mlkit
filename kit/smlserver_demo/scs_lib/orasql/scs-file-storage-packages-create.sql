create or replace package scs_file_storage
as

  /* [newFsInstance label fs_type_id last_mod_user] creates a new File
      Storage Instance and returns an instance_id. Returns null if an
      instance named label already exists. */
  function newFsInstance (
    label         in scs_fs_instances.label%TYPE,
    fs_type_id    in scs_fs_instances.fs_type_id%TYPE,
    last_mod_user in scs_fs_instances.last_modifying_user%TYPE
  ) return scs_fs_types.type_id%TYPE;

  /* [newFsType name max_files max_revisions max_filesize] creates a
      new File Storage Type and returns a type_id. Returns null if a
      type named name already exists. */
  function newFsType (
    name          in scs_fs_types.name%TYPE,
    max_files     in scs_fs_types.max_files%TYPE,
    max_revisions in scs_fs_types.max_revisions%TYPE,
    max_filesize  in scs_fs_types.max_filesize%TYPE
  ) return scs_fs_types.type_id%TYPE;

  /* [getFsTypeIdByName name] returns the type id for the file storage
      type with name name (name is unique). Returns null if no type
      with name exists. */
  function getFsTypeIdByName (
    name in scs_fs_types.name%TYPE
  ) return scs_fs_types.type_id%TYPE;

  /* [getMimeTypeIdByFileExt file_ext] returns the mime type id for
      the mime type with file extension file_ext (file_ext is
      unique). Returns null if no file extension exists. */
  function getMimeTypeIdByFileExt (
    file_ext in scs_fs_mime_types.file_extension%TYPE
  ) return scs_fs_mime_types.id%TYPE;

  /* [addMimeTypeToFsType fs_type_id mime_type_id] adds mime_type_id
      to the File Storage type fs_type_id. The function fails if
      either fs_type_id or mime_type_id does not exists. It is not an
      error to add the same mime type several times. */
  procedure addMimeTypeToFsType (
    fs_type_id in scs_fs_types.type_id%TYPE,
    mime_type_id in scs_fs_mime_types.id%TYPE
  );

  /* [getInstanceId root_label] returns the instance_id for the file
     storage instance labelled root_label. Returns null if root_label
     does not exists. */
  function getInstanceId (
    root_label in scs_fs_instances.label%TYPE
  ) return scs_fs_instances.instance_id%TYPE;

  /* [getInstanceIdByFolderId root_label] returns the instance_id for
     the folder with id folder_id. Returns null if root_label does not
     exists. */
  function getInstanceIdByFolderId (
    folder_id in scs_fs_folders.folder_id%TYPE
  ) return scs_fs_instances.instance_id%TYPE;

  /* [getRootFolderId root_label folder_name] returns the root folder
      id of root folder folder_name. Returns null if no root folder
      named folder_name in instance root_label exists. */
  function getRootFolderId (
    root_label in scs_fs_instances.label%TYPE,
    folder_name in scs_fs_folders.foldername%TYPE
  ) return scs_fs_folders.folder_id%TYPE;

  /* [getSubFolderId parent_folder_id folder_name] returns the folder
      id of folder_name in folder with folder_id =
      parent_folder_id. Returns null if no parent folder or
      folder_name exists. */
  function getSubFolderId (
    parent_folder_id in scs_fs_folders.folder_id%TYPE,
    folder_name in scs_fs_folders.foldername%TYPE
  ) return scs_fs_folders.folder_id%TYPE;
end scs_file_storage;
/
show errors


/* ======================================================================
   package bodies start here
====================================================================== */ 
create or replace package body scs_file_storage
as
  function newFsInstance (
    label         in scs_fs_instances.label%TYPE,
    fs_type_id    in scs_fs_instances.fs_type_id%TYPE,
    last_mod_user in scs_fs_instances.last_modifying_user%TYPE
  ) return scs_fs_types.type_id%TYPE
  is
    instance_id scs_fs_instances.instance_id%TYPE;
  begin
    instance_id := scs.new_obj_id();

    insert into scs_fs_instances
      (instance_id,label,fs_type_id,last_modifying_user)
    values 
      (newFsInstance.instance_id,
       newFsInstance.label,
       newFsInstance.fs_type_id,
       newFsInstance.last_mod_user);

    return newFsInstance.instance_id;

  exception
    when others then
      return null;
  end newFsInstance;


  function newFsType (
    name          in scs_fs_types.name%TYPE,
    max_files     in scs_fs_types.max_files%TYPE,
    max_revisions in scs_fs_types.max_revisions%TYPE,
    max_filesize  in scs_fs_types.max_filesize%TYPE
  ) return scs_fs_types.type_id%TYPE
  is
    type_id scs_fs_types.type_id%TYPE;
  begin 
    type_id := scs.new_obj_id();

    insert into scs_fs_types
      (type_id, name, max_files, max_revisions, max_filesize)
    values (newFsType.type_id,newFsType.name,newFsType.max_files,
            newFsType.max_revisions,newFsType.max_filesize);

    return type_id;

  exception
    when others then
      return null;
  end newFsType;

  function getFsTypeIdByName (
    name in scs_fs_types.name%TYPE
  ) return scs_fs_types.type_id%TYPE
  is
    res scs_fs_types.type_id%TYPE;
  begin
    select type_id
      into res
      from scs_fs_types
     where scs_fs_types.name = getFsTypeIdByName.name;

    return res;

  exception
    when others then
      return null;
  end getFsTypeIdByName;

  function getMimeTypeIdByFileExt (
    file_ext in scs_fs_mime_types.file_extension%TYPE
  ) return scs_fs_mime_types.id%TYPE
  is
    res scs_fs_mime_types.id%TYPE;
  begin
    select id
      into getMimeTypeIdByFileExt.res
      from scs_fs_mime_types
     where file_extension = getMimeTypeIdByFileExt.file_ext;

    return res;

  exception
    when others then
      return null;
  end getMimeTypeIdByFileExt;


  procedure addMimeTypeToFsType (
    fs_type_id in scs_fs_types.type_id%TYPE,
    mime_type_id in scs_fs_mime_types.id%TYPE
  )
  is 
  begin 
    insert into scs_fs_type_mime_type_map
      (rel_id,fs_type_id,mime_type_id)
    values
      (scs.new_obj_id(),
       addMimeTypeToFsType.fs_type_id,
       addMimeTypeToFsType.mime_type_id);

    return;
  exception
    when DUP_VAL_ON_INDEX then
      return;
    when others then
      raise_application_error(scs.ScsDbExn,'scs_file_storage.addMimeTypeToFsType: either fs_type_id: ' 
        || to_char(addMimeTypeToFsType.fs_type_id) || ' or mime_type_id: ' 
        || to_char(addMimeTypeToFsType.mime_type_id) || ' does not exist.');
  end addMimeTypeToFsType;

  function getInstanceId(
    root_label in scs_fs_instances.label%TYPE
  ) return scs_fs_instances.instance_id%TYPE
  is
    id integer;
  begin 
    select instance_id
      into getInstanceId.id
      from scs_fs_instances
     where label = getInstanceId.root_label;

    return id;

    exception
      when others then
        return null;
  end getInstanceId;

  function getInstanceIdByFolderId (
    folder_id in scs_fs_folders.folder_id%TYPE
  ) return scs_fs_instances.instance_id%TYPE
  is
    id integer;
  begin 
    select instance_id
      into id
      from scs_fs_folders
     where folder_id = getInstanceIdByFolderId.folder_id;

    return id;

    exception
      when others then
        return null;
  end getInstanceIdByFolderId;

  function getRootFolderId (
    root_label in scs_fs_instances.label%TYPE,
    folder_name in scs_fs_folders.foldername%TYPE
  ) return scs_fs_folders.folder_id%TYPE
  is
    v_id scs_fs_folders.folder_id%TYPE;
  begin
    select folder_id
      into v_id
      from scs_fs_folders, scs_fs_instances
     where scs_fs_folders.instance_id = scs_fs_instances.instance_id
       and scs_fs_instances.label = getRootFolderId.root_label
       and scs_fs_folders.parent_id is null
       and scs_fs_folders.foldername = getRootFolderId.folder_name;

    return v_id;

  exception
    when others then
      return null;
  end getRootFolderId;

  function getSubFolderId (
    parent_folder_id in scs_fs_folders.folder_id%TYPE,
    folder_name in scs_fs_folders.foldername%TYPE
  ) return scs_fs_folders.folder_id%TYPE
  is
    id scs_fs_folders.folder_id%TYPE;
  begin 
    select folder_id
      into id
      from scs_fs_folders
     where scs_fs_folders.parent_id = getSubFolderId.parent_folder_id
       and scs_fs_folders.foldername = getSubFolderId.folder_name;

    return id;

  exception
    when others then
      return null;
  end getSubFolderId;


end scs_file_storage;
/
show errors
