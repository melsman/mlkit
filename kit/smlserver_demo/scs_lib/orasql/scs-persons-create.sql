-- This code i a down-sized and modified version of the persons module
-- found in openACS version 4.5b1 (www.openacs.org): file
-- community-core-create.sql.

create table scs_persons (
  person_id integer
    constraint scs_persons_person_id_nn not null
    constraint scs_persons_person_id_fk references scs_parties(party_id)
    constraint scs_persons_pk primary key,
  first_names varchar2(100) 
    constraint scs_persons_first_names_nn not null,
  last_name varchar2(100)
    constraint scs_persons_last_name_nn  not null,
  security_id varchar2(50),
  last_modified date default sysdate 
    constraint scs_persons_last_modified_nn not null,
  modifying_user integer,
  deleted_p char(1) 
    constraint scs_persons_deleted_p_nn not null
    constraint scs_persons_deleted_p_ck check (deleted_p in ('t','f'))
);

create table scs_person_rels (
  person_id integer
    constraint scs_person_rels_person_id_nn not null
    constraint scs_person_rels_person_id_fk references scs_persons(person_id)
    constraint scs_person_rels_person_id_pk primary key,
  on_what_table varchar(100)
    constraint scs_person_rels_on_w_tab_nn not null,
  on_which_id varchar(100)
    constraint scs_person_rels_on_w_id_nn not null
);

comment on table scs_persons is '
';

-------------------------
-- Package Description --
-------------------------
create or replace package scs_person
as
  function new (
    person_id      in scs_persons.person_id%TYPE default null,
    email          in scs_parties.email%TYPE,
    url	           in scs_parties.url%TYPE default null,
    first_names	   in scs_persons.first_names%TYPE,
    last_name	   in scs_persons.last_name%TYPE,
    security_id    in scs_persons.security_id%TYPE default null,
    last_modified  in scs_persons.last_modified%TYPE default sysdate,
    modifying_user in scs_persons.modifying_user%TYPE,
    deleted_p      in scs_persons.deleted_p%TYPE default 'f'
  ) return scs_persons.person_id%TYPE;

  procedure delete (
    person_id in scs_persons.person_id%TYPE
  );

  function first_names (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.first_names%TYPE;

  function last_name (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.last_name%TYPE;

end scs_person;
/
show errors

------------------
-- Package Body --
------------------

create or replace package body scs_person
as
  function new (
    person_id      in scs_persons.person_id%TYPE default null,
    email          in scs_parties.email%TYPE,
    url	           in scs_parties.url%TYPE default null,
    first_names	   in scs_persons.first_names%TYPE,
    last_name	   in scs_persons.last_name%TYPE,
    security_id    in scs_persons.security_id%TYPE default null,
    last_modified  in scs_persons.last_modified%TYPE default sysdate,
    modifying_user in scs_persons.modifying_user%TYPE,
    deleted_p      in scs_persons.deleted_p%TYPE default 'f'
  ) return scs_persons.person_id%TYPE
  is
    v_person_id scs_persons.person_id%TYPE;
  begin
    v_person_id := scs_party.new(person_id,'scs_persons',email,url,last_modified,modifying_user,deleted_p);

    insert into scs_persons (person_id, first_names, last_name, security_id, 
                             last_modified, modifying_user, deleted_p)
    values (v_person_id, first_names, last_name, security_id, last_modified, 
            modifying_user, deleted_p);

    return v_person_id;
  end new;

  procedure delete (
    person_id in scs_persons.person_id%TYPE
  )
  is
  begin
    update scs_persons
       set deleted_p = 't'
     where person_id = scs_person.delete.person_id;

    scs_party.delete(person_id);
  end delete;

  function name (
    person_id in scs_persons.person_id%TYPE
  ) return varchar2
  is
    v_person_name varchar2(200);
  begin
    select first_names || ' ' || last_name
      into v_person_name
      from scs_persons
     where person_id = scs_person.name.person_id;

    return v_person_name;
  end name;

  function first_names (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.first_names%TYPE
  is
    v_person_first_names varchar2(200);
  begin
    select first_names
      into v_person_first_names
      from scs_persons
     where person_id = scs_person.first_names.person_id;

    return v_person_first_names;
  end first_names;

  function last_name (
    person_id	in scs_persons.person_id%TYPE
  ) return scs_persons.last_name%TYPE
  is
    v_person_last_name varchar2(200);
  begin
    select last_name
      into v_person_last_name
      from scs_persons
     where person_id = scs_person.last_name.person_id;

    return v_person_last_name;
  end last_name;
end scs_person;
/
show errors
