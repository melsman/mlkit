-- This code i a down-sized and modified version of the party module found
-- in openACS (www.openacs.org): file community-core-create.sql.

create table scs_parties (
  party_id integer
    constraint scs_parties_party_id_nn not null
    constraint scs_parties_pk primary key,
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

-- DRB: I added this trigger to enforce the storing of e-mail in lower case.
-- party.new() already did so but I found an update that didn't...

create or replace trigger scs_parties_in_up_tr
before insert or update on scs_parties
for each row
begin
  :new.email := lower(:new.email);
end;
/
show errors

-------------------
-- PARTY PACKAGE --
-------------------

create or replace package party
as
  function new (
    party_id	   in scs_parties.party_id%TYPE default null,
    email	   in scs_parties.email%TYPE,
    url		   in scs_parties.url%TYPE default null,
    last_modified  in scs_parties.last_modified%TYPE default sysdate,
    modifying_user in scs_parties.modifying_user%TYPE default null,
    deleted_p      in scs_parties.deleted_p%TYPE default 'f'
  ) return scs_parties.party_id%TYPE;

  procedure delete (
    party_id in scs_parties.party_id%TYPE
  );

  function email (
    party_id in scs_parties.party_id%TYPE
  ) return varchar2;

end party;
/
show errors

create or replace package body party
as
  function new (
    party_id	   in scs_parties.party_id%TYPE default null,
    email	   in scs_parties.email%TYPE,
    url		   in scs_parties.url%TYPE default null,
    last_modified  in scs_parties.last_modified%TYPE default sysdate,
    modifying_user in scs_parties.modifying_user%TYPE default null,
    deleted_p      in scs_parties.deleted_p%TYPE default 'f'
  )
  return scs_parties.party_id%TYPE
  is
    v_party_id scs_parties.party_id%TYPE;
  begin
    v_party_id := scs.new_obj_id(party_id);

    insert into scs_parties (party_id, email, url)
      values (v_party_id, lower(email), url);

    return v_party_id;
  end new;

  procedure delete (
    party_id in scs_parties.party_id%TYPE
  )
  is
  begin
    update scs_parties
       set deleted_p = 't'
     where scs_parties.party_id = party.delete.party_id;
  end delete;

  function email (
    party_id in scs_parties.party_id%TYPE
  ) return varchar2
  is
    v_email scs_parties.email%TYPE;
  begin
    select email
      into v_email
      from scs_parties
     where party_id = email.party_id;

    return v_email;
 end email;

end party;
/
show errors


