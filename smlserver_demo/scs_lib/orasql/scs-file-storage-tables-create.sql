create sequence scs_fs_mime_types_id_seq start with 10;
create table scs_fs_mime_types (
  id integer
    constraint scs_fs_mime_types_id_pk primary key
    constraint scs_fs_mime_types_id_nn not null,
  label	varchar2(200)
    constraint scs_fs_mime_types_label_nn not null,
  mime_type varchar2(200)
    constraint scs_fs_mime_types_mime_type_un unique
    constraint scs_fs_mime_types_mime_type_nn not null,    
  file_extension varchar2(200)
    constraint scs_fs_mime_types_file_ext_un unique,
  file_icon varchar2(200)
);

create table scs_fs_types (
  type_id integer
    constraint scs_fs_types_type_id_id_nn not null
    constraint scs_fs_types_type_id_id_pk primary key,
  name varchar(200)
    constraint scs_fs_types_name_nn not null
    constraint scs_fs_types_name_un unique,
  max_files integer
    constraint scs_fs_types_type_max_files_nn not null,
  max_revisions integer
    constraint scs_fs_types_type_max_rev_nn not null,
  max_filesize integer
    constraint scs_fs_types_type_max_fsz_nn not null
);

create table scs_fs_type_mime_type_map (
  rel_id integer
    constraint scs_fs_type_mime_t_m_rel_id_ok primary key
    constraint scs_fs_type_mime_t_m_rel_id_nn not null,  
  fs_type_id integer
    constraint scs_fs_type_mime_t_m_fs_tid_fk
      references scs_fs_types(type_id)
    constraint scs_fs_type_mime_t_m_fs_tid_nn not null,    
  mime_type_id integer
    constraint scs_fs_type_mime_t_m_mtid_fk
      references scs_fs_mime_types(id)
    constraint scs_fs_type_mime_t_m_mtid_nn not null,
  constraint scs_fs_type_mime_t_m_un
    unique (fs_type_id,mime_type_id)
);

create table scs_fs_instances (
  instance_id integer 
    constraint scs_fs_intances_id_nn not null
    constraint scs_fs_intances_id_pk primary key,
  -- label is the root directory for this instance.
  label varchar(200)
    constraint scs_fs_instances_label_nn not null
    constraint scs_fs_instances_label_un unique,
  fs_type_id integer
    constraint scs_fs_instances_fs_type_id_nn not null
    constraint scs_fs_instances_fs_type_id_fk 
      references scs_fs_types(type_id),
  deleted_p char(1) default 'f'
    constraint scs_fs_instances_del_p_nn not null
    constraint scs_fs_instances_del_p_ck 
      check (deleted_p in ('t','f')),
  last_modified date default sysdate
    constraint scs_fs_instances_last_mod_nn not null,
  last_modifying_user integer
    constraint scs_fs_inst_last_mod_user_nn not null 
    constraint scs_fs_inst_last_mod_user_rf 
      references scs_users
);  

create table scs_fs_folders (
  folder_id integer
    constraint scs_fs_folders_id_nn not null
    constraint scs_fs_folders_id_pk primary key,
  instance_id integer
    constraint scs_fs_folders_instance_id_nn not null
    constraint scs_fs_folders_instance_id_fk
      references scs_fs_instances(instance_id),
  parent_id integer
    constraint scs_fs_folders_parent_id_fk
      references scs_fs_folders(folder_id),
  -- label is the name used on the disk.
  label varchar2(200)
    constraint scs_fs_folders_label_nn not null,
  -- foldername is the name shown on the screen.
  foldername varchar2(200)
    constraint scs_fs_folders_foldername_nn not null,
  deleted_p char(1) default 'f'
    constraint scs_fs_folders_del_p_nn not null
    constraint scs_fs_folders_del_p_ck 
      check (deleted_p in ('t','f')),
  last_modified date default sysdate
    constraint scs_fs_folders_last_mod_nn not null,
  last_modifying_user integer
    constraint scs_fs_fold_last_mod_user_nn not null 
    constraint scs_fs_fold_last_mod_user_rf 
      references scs_users,
  -- Sub-folders must have different names in the same folder
  constraint scs_fs_folders_label_un unique (parent_id,label),
  constraint scs_fs_folders_folder_un unique (parent_id,foldername)
);  

create table scs_fs_files (
  file_id integer
    constraint scs_fs_files_file_id_nn not null
    constraint scs_fs_files_file_id_pk primary key,
  folder_id integer
    constraint scs_fs_files_folder_id_nn not null
    constraint scs_fs_files_folder_id_fk 
      references scs_fs_folders(folder_id),
  name varchar2(200)
    constraint scs_fs_files_name_nn not null,
  description varchar(1000)
    constraint scs_fs_files_desc_nn not null,
  deleted_p char(1) default 'f'
    constraint scs_fs_files_del_p_nn not null
    constraint scs_fs_files_del_p_ck
      check (deleted_p in ('t','f')),
  mime_type_id integer
    constraint scs_fs_files_mime_type_id_nn not null
    constraint scs_fs_files_mime_type_id_fk
      references scs_fs_mime_types(id),
  last_modified date default sysdate
    constraint scs_fs_files_last_mod_nn not null,
  last_modifying_user integer
    constraint scs_fs_files_last_mod_user_nn not null 
    constraint scs_fs_files_last_mod_user_rf 
      references scs_users,
  -- Files must have different names in the same folder
  constraint scs_fs_files_un unique(folder_id,name)
);

create table scs_fs_revisions (
  revision_id integer
    constraint scs_fs_rev_nn not null
    constraint scs_fs_rev_pk primary key,
  file_id integer
    constraint scs_fs_rev_file_id_nn not null
    constraint scs_fs_rev_file_id_fk 
      references scs_fs_files(file_id),
  mime_type_id integer
    constraint scs_fs_rev_mime_type_id_nn not null
    constraint scs_fs_rev_mime_type_id_fk 
      references scs_fs_mime_types(id),
  -- filename on disk
  filename varchar2(4000),
  -- filesize is in Kb.
  filesize integer
    constraint scs_fs_rev_fsz_nn not null,
  -- file_delted_p is t if file has been deleted from disk.
  file_deleted_p char(1) default 'f'
    constraint scs_fs_rev_file_del_p_nn not null
    constraint scs_fs_rev_file_del_p_ck 
      check (file_deleted_p in ('t','f')),
  deleted_p char(1) default 'f'
    constraint scs_fs_rev_del_p_nn not null
    constraint scs_fs_rev_del_p_ck 
      check (deleted_p in ('t','f')),
  last_modified date default sysdate
    constraint scs_fs_rev_last_mod_nn not null,
  last_modifying_user integer
    constraint scs_fs_rev_last_mod_user_nn not null 
    constraint scs_fs_rev_last_mod_user_rf 
      references scs_users
);

