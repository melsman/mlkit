-- $Id$

-- This code i a down-sized and modified version of the party module found
-- in openACS (www.openacs.org): file community-core-create.sql.

create table scs_parties (
  party_id integer
    constraint scs_parties_party_id_nn not null
    constraint scs_parties_pk primary key,
  party_type varchar(100)
    constraint scs_parties_party_type_ck
      check(party_type in ('scs_persons','scs_groups')),
  email	varchar2(100)
    constraint scs_parties_email_un unique,
  url varchar2(200),
  last_modified date default sysdate 
    constraint scs_parties_last_modified_nn not null,
  modifying_user integer,
  deleted_p char(1) default 'f'
    constraint scs_parties_deleted_p_nn not null
    constraint scs_parties_deleted_p_ck check (deleted_p in ('t','f'))
);

comment on table scs_parties is '
 Party is the supertype of person and organization. It exists because
 many other types of object can have relationships to scs_parties.
';

comment on column scs_parties.url is '
 We store url here so that we can always make party names hyperlinks
 without joining to any other table.
';


