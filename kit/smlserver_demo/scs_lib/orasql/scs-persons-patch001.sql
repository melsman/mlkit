/* 
Patch 001 for scs-persons

Date:   2003-11-02
Author: Niels Hallenberg
Purpose: 
* Add table scs_profiles for storing user profiles
* Add fields to scs_parties to allow for upload of portraits.
* Add table scs_portraits to allow for upload of portraits

*/

create table scs_profiles (
  party_id integer
    constraint scs_profiles_party_id_nn not null
    constraint scs_profiles_party_id_pk primary key
    constraint scs_profiles_party_id_fk
      references scs_parties(party_id),
  profile_tid integer
    constraint scs_profiles_profile_fk 
      references scs_texts(text_id),
  keywords_tid integer
    constraint scs_profiles_keywords_fk 
      references scs_texts(text_id),
  -- edit_no used to prevent unsyncronised updates
  edit_no integer default 0
    constraint ucs_profiles_edit_no_nn not null,
  last_modified date default sysdate 
    constraint scs_profiles_last_modified_nn not null,
  modifying_user integer,
  deleted_p char(1) default 'f'
    constraint scs_profiles_deleted_p_nn not null
    constraint scs_profiles_deleted_p_ck 
      check (deleted_p in ('t','f'))
);

