/* 
Patch 001 for scs-parties

Date:   2003-11-04
Author: Niels Hallenberg
Purpose: 
* Add FS instance for the uploaded portraits.
* Add fields to scs_parties to allow for upload of portraits.
* Add enumeration for portrait_type.
* Add table scs_portraits to allow for upload of portraits
*/

alter table scs_parties
  add upload_folder_id int;
alter table scs_parties
  add upload_folder_name varchar(200);
alter table scs_parties
  add upload_folder_path varchar(200);
alter table scs_parties
  add may_show_portrait_p char(1) default 'f';
alter table scs_parties
  modify may_show_portrait_p char(1) default 'f' not null; 

alter table scs_parties
  add constraint scs_parties_upl_f_id_fk 
    foreign key (upload_folder_id)
      references scs_fs_folders(folder_id);
alter table scs_parties
  add constraint scs_parties_may_show_p_p_ck 
    check (may_show_portrait_p in ('t','f'));

declare
  type_id scs_fs_types.type_id%TYPE;
  instance_id scs_fs_instances.instance_id%TYPE;
begin
  type_id := scs_file_storage.newFsType(name => 'ScsPersonPortrait',
                                        max_files => 6,
                                        max_revisions => 1,
                                        max_filesize => 384);
  scs_file_storage.addMimeTypeToFsType(fs_type_id => type_id,
                                       mime_type_id => scs_file_storage.getMimeTypeIdByFileExt('png'));
  scs_file_storage.addMimeTypeToFsType(fs_type_id => type_id,
                                       mime_type_id => scs_file_storage.getMimeTypeIdByFileExt('jpg'));
  scs_file_storage.addMimeTypeToFsType(fs_type_id => type_id,
                                       mime_type_id => scs_file_storage.getMimeTypeIdByFileExt('gif'));
  instance_id := scs_file_storage.newFsInstance(label => 'ScsPersonPortrait',
                                                fs_type_id => type_id,
                                                last_mod_user => 
                                                scs_party.partyIdByEmail(email => 'nh@itu.dk'));
end;
/ 
show errors

declare
  eid scs_enumerations.enum_id%TYPE;

  tid scs_texts.text_id%TYPE;

  vid scs_enum_values.val_id%TYPE;
begin
  /* ========================================
  creating portrait types
  ======================================== */
  eid := scs_enumeration.new( name => scs_party.scs_portrait_types );

  tid := scs_text.updateText( 
    language  => 'da', text  => 'Original',
    language2 => 'en', text2 => 'Original');
  vid := scs_enumeration.updateValue(
    enum_id => eid, text_id => tid, value => 'orig');
  tid := scs_text.updateText( 
    language  => 'da', text  => 'Thumbnail (fast højde)',
    language2 => 'en', text2 => 'Thumbnail (fixed height)');
  vid := scs_enumeration.updateValue(
    enum_id => eid, text_id => tid, value => 'thumb_fixed_height');
  tid := scs_text.updateText( 
    language  => 'da', text  => 'Thumbnail (fast bredde)',
    language2 => 'en', text2 => 'Thumbnail (fixed width)' );
  vid := scs_enumeration.updateValue(
    enum_id => eid, text_id => tid, value => 'thumb_fixed_width' );
end;
/ 
show errors

create table scs_portraits (
  file_id integer
    constraint scs_portraits_file_id_nn not null
    constraint scs_portraits_file_id_pk primary key
    constraint scs_portraits_file_id_fk
      references scs_fs_files(file_id),
  party_id integer
    constraint scs_portraits_party_id_nn not null
    constraint scs_portraits_party_id_fk 
      references scs_parties(party_id),
  portrait_type_vid integer
    constraint scs_portraits_type_vid_nn not null
    constraint scs_portraits_type_vid_fk 
      references scs_enum_values(val_id),
  width integer
    constraint scs_portraits_width_nn not null,
  height integer
    constraint scs_portraits_height_nn not null,
  bytes integer
    constraint scs_portraits_bytes_nn not null,
  official_p char(1) default 'f'
    constraint scs_portraits_official_p_nn not null
    constraint scs_portraits_official_p_ck
      check (official_p in ('t','f')),
  constraint scs_portraits_un 
    unique (party_id,portrait_type_vid,official_p)
);

