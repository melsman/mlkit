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
  norm_name varchar2(200)
    constraint scs_persons_norm_name_nn not null,
  security_id varchar2(50),
  last_modified date default sysdate 
    constraint scs_persons_last_modified_nn not null,
  modifying_user integer,
  deleted_p char(1) 
    constraint scs_persons_deleted_p_nn not null
    constraint scs_persons_deleted_p_ck check (deleted_p in ('t','f'))
);

/* We enforce that an external row may only relate to one person! */
create table scs_person_rels (
  person_id integer
    constraint scs_person_rels_person_id_nn not null
    constraint scs_person_rels_person_id_fk references scs_persons(person_id),
  on_what_table varchar(100)
    constraint scs_person_rels_on_w_tab_nn not null,
  on_which_id integer
    constraint scs_person_rels_on_w_id_nn not null,
  constraint scs_person_rels_pk primary key (on_what_table,on_which_id)
);

----------------------------------------
-- Package scs_person_rel Description --
----------------------------------------
create or replace package scs_person_rel
as
  procedure add (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  );

  procedure delete (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  );

  procedure delete_all (
    person_id     in scs_person_rels.person_id%TYPE
  );

  function exists_p (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  ) return char;
end scs_person_rel;
/
show errors

---------------------------------
-- Package scs_person_rel Body --
---------------------------------

create or replace package body scs_person_rel
as
  procedure add (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  )
  is
  begin
    insert into scs_person_rels (person_id,on_what_table,on_which_id)
      values (add.person_id,add.on_what_table,add.on_which_id);
  exception
    when dup_val_on_index then
      return; /* We silently ignore that the relation is already there */
    when others then
      raise_application_error(scs.ScsDbExn,'scs_person_rel.add: Can''t add person_id ' || 
                              person_id || ' to scs_person_rels.');
  end add;

  procedure delete (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  )
  is
    tmp integer;
  begin
    delete scs_person_rels
     where person_id = scs_person_rel.delete.person_id
       and on_what_table = scs_person_rel.delete.on_what_table
       and on_which_id = scs_person_rel.delete.on_which_id;
     
  exception
    when others then
      return; /* We silently ignore that the relation is not there */
  end delete;

  procedure delete_all (
    person_id     in scs_person_rels.person_id%TYPE
  )
  is
    tmp integer;
  begin
    delete scs_person_rels
     where person_id = scs_person_rel.delete_all.person_id;
  exception
    when others then
      return; /* We silently ignore that the relation is not there */
  end delete_all;

  function exists_p (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  ) return char
  is
    res char;
  begin
    select decode(count(*),0,'f','t')
      into res
      from scs_person_rels
     where scs_person_rels.person_id = exists_p.person_id
       and scs_person_rels.on_what_table = exists_p.on_what_table
       and scs_person_rels.on_which_id = exists_p.on_which_id;

    return res;
  end exists_p;

end scs_person_rel;
/
show errors

------------------------------------
-- Package scs_person Description --
------------------------------------
create or replace package scs_person
as
  function new (
    person_id      in scs_persons.person_id%TYPE default null,
    email          in scs_parties.email%TYPE default null,
    url	           in scs_parties.url%TYPE default null,
    first_names	   in scs_persons.first_names%TYPE,
    last_name	   in scs_persons.last_name%TYPE,
    security_id    in scs_persons.security_id%TYPE default null,
    last_modified  in scs_persons.last_modified%TYPE default sysdate,
    modifying_user in scs_persons.modifying_user%TYPE,
    deleted_p      in scs_persons.deleted_p%TYPE default 'f'
  ) return scs_persons.person_id%TYPE;

  procedure destroy (
    person_id in scs_persons.person_id%TYPE
  );

  function deleted_p (
    person_id in scs_persons.person_id%TYPE
  ) return char;

  function first_names (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.first_names%TYPE;

  function last_name (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.last_name%TYPE;

  function name (
    person_id in scs_persons.person_id%TYPE
  ) return varchar2;
  
  function security_id (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.security_id%TYPE;

  function norm_name(
    first_names in scs_persons.first_names%TYPE,
    last_name   in scs_persons.last_name%TYPE
  ) return varchar2;

  function norm_name(
    person_id in scs_persons.person_id%TYPE
  ) return varchar2;

  function norm_name_exists_p (
    norm_name in scs_persons.norm_name%TYPE
  ) return char;
end scs_person;
/
show errors

-----------------------------
-- Package scs_person Body --
-----------------------------

create or replace package body scs_person
as
  function new (
    person_id      in scs_persons.person_id%TYPE default null,
    email          in scs_parties.email%TYPE default null,
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

  exception
    when others then
      raise_application_error(scs.ScsDbExn,'can''t insert dublicate person_id');
  end new;

  procedure destroy (
    person_id in scs_persons.person_id%TYPE
  )
  is
  begin
    /* Physically delete all relations */
    scs_person_rel.delete_all(destroy.person_id);
    /* Mark person as deleted */
    update scs_persons
       set deleted_p = 't'
     where person_id = scs_person.destroy.person_id;

    scs_party.destroy(person_id);
  end destroy;

  function deleted_p (
    person_id in scs_persons.person_id%TYPE
  ) return char
  is
    v_deleted_p char;
  begin
    select deleted_p
      into v_deleted_p
      from scs_persons
     where scs_persons.person_id = deleted_p.person_id;
  
    return v_deleted_p;
  end deleted_p;

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

  function security_id (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.security_id%TYPE
  is
    v_person_security_id varchar2(200);
  begin
    select security_id
      into v_person_security_id
      from scs_persons
     where person_id = scs_person.security_id.person_id;

    return v_person_security_id;
  end security_id;

  /* A condensed version of the person's name: middle names and spaces
  are omitted and all letters are converted to lower space. Example
  value: 'madstofte', which would be a normalised version of 'Mads
  Hugo Henrik Tofte', for example.

  To avoid that the same person is added to the data base twice, all
  applications that insert new people into the data base are supposed
  to normalise the name of the person that is to be inserted and check
  against the normalised names that are already in the data base. If
  there is a match, the application must show the information of the
  matching person(s) to the user and ask whether the new person is
  really new. Only if the user accepts, should the new person be
  accepted.  */
  function norm_name(
    first_names in scs_persons.first_names%TYPE,
    last_name   in scs_persons.last_name%TYPE
  ) return varchar2
  is
    v_norm_name varchar2(200);
  begin
    v_norm_name := ltrim(first_names,' ') || ' ';
    v_norm_name := substr(v_norm_name,1,instr(v_norm_name,' ')-1);
    v_norm_name := v_norm_name || replace(last_name,' ');
  
    return lower(v_norm_name);
  end norm_name;

  function norm_name(
    person_id in scs_persons.person_id%TYPE
  ) return varchar2
  is 
    v_norm_name scs_persons.norm_name%TYPE;
  begin
    select norm_name
      into v_norm_name
      from scs_persons
     where scs_persons.person_id = norm_name.person_id;

    return v_norm_name;
  end norm_name;

  function norm_name_exists_p (
    norm_name in scs_persons.norm_name%TYPE
  ) return char
  is
    res char(1);
  begin
    select decode(count(*),0,'f','t')
      into res
      from scs_persons
     where scs_persons.norm_name = norm_name_exists_p.norm_name
       and scs_persons.deleted_p = 'f';

    return res;
  end norm_name_exists_p;

end scs_person;
/
show errors

--------------
-- Triggers --
--------------

create or replace trigger scs_persons_in_up_tr
before insert or update on scs_persons
for each row
begin
  :new.norm_name := scs_person.norm_name(:new.first_names,:new.last_name);
end;
/
show errors
