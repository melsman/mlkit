-- $Id$

-- This code i a down-sized and modified version of the persons module
-- found in openACS version 4.5b1 (www.openacs.org): file
-- community-core-create.sql.


/* ======================================================================
   package scs_person

   History:
   151102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   281002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_person
as
  /* ------------
     function new
     ------------
     creates a new person and returns person_id
     Throws a ScsDbExn exception if there is exists a person with email 'email'
       or if there exists an entry with person_id 'person_id'
  */
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


  /* -----------------
     procedure destroy
     -----------------
     Physically deletes all relations in scs_person_rels,
     marks person as deleted in scs_persons and
     calls scs_party.destroy on the person_id

     never fails
  */
  procedure destroy (
    person_id in scs_persons.person_id%TYPE
  );


  /* ------------------
     function deleted_p
     ------------------
     returns 
       't'/'f' for person_id
     or	     
       null if person_id doesn't exist     
  */
  function deleted_p (
    person_id in scs_persons.person_id%TYPE
  ) return char;


  /* --------------------
     function first_names
     --------------------
     returns 
       first_names for person_id
     or	     
       null  if person_id doesn't exist     
  */
  function first_names (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.first_names%TYPE;


  /* ------------------
     function last_name
     ------------------
     returns 
       last_name for person_id
     or	     
       null  if person_id doesn't exist     
  */
  function last_name (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.last_name%TYPE;


  /* -------------
     function name
     -------------
     returns 
       a string representation of first_names, last_name for person_id
     or	     
       null  if person_id doesn't exist     
  */
  function name (
    person_id in scs_persons.person_id%TYPE
  ) return varchar2;
  

  /* --------------------
     function security_id
     --------------------
     returns 
       security_id for person_id
     or	     
       null  if person_id doesn't exist     
  */
  function security_id (
    person_id in scs_persons.person_id%TYPE
  ) return scs_persons.security_id%TYPE;


  /* ------------------
     function norm_name
     ------------------
     Returns condensed version of the person's name: 
     middle names and spaces are omitted and all letters are converted 
     to lower space. 
       Example: the normalised version of 'Mads Hugo Henrik Tofte'  
                would be 'madstofte'

    To avoid that the same person is added to the data base twice, all
    applications that insert new people into the data base are supposed
    to normalise the name of the person that is to be inserted and check
    against the normalised names that are already in the data base. If
    there is a match, the application must show the information of the
    matching person(s) to the user and ask whether the new person is
    really new. Only if the user accepts, should the new person be
    accepted.
    
    returns null if first_names and last_name are both null
  */
  function norm_name(
    first_names in scs_persons.first_names%TYPE,
    last_name   in scs_persons.last_name%TYPE
  ) return varchar2;


  /* ------------------
     function norm_name
     ------------------
     returns 
       the normalised name for person_id
     or	     
       null  if person_id doesn't exist     
  */
  function norm_name(
    person_id in scs_persons.person_id%TYPE
  ) return varchar2;


  /* ---------------------------
     function norm_name_exists_p
     ---------------------------
     returns 't' if there exist a person with normalised name 'norm_name'
     returns 'f' if not
  */
  function norm_name_exists_p (
    norm_name in scs_persons.norm_name%TYPE
  ) return char;
end scs_person;
/
show errors


/* ======================================================================
   package scs_person_rel

   History:
   151102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   281002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_person_rel
as
  /* -------------
     procedure add
     -------------
     adds the relation (person_id, on_what_table, on_which_id)
     to the table scs_person_rels

     if the relation is already there, nothing happens

     Throws a ScsDbExn if person_id is unknown
  */  
  procedure add (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  );

  /* ----------------
     procedure delete
     ----------------
     deletes the relation (person_id, on_what_table, on_which_id)
     from the table scs_person_rels

     never fails     
  */
/* delete is a reserved word in Oracle 9i!
  procedure delete (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  );
*/

  /* --------------------
     procedure delete_all
     --------------------
     deletes all relations (person_id, on_what_table, on_which_id)
     from the table scs_person_rels for person_id

     never fails     
  */
  procedure delete_all (
    person_id     in scs_person_rels.person_id%TYPE
  );

  /* ---------------------------
     function exists_p
     ---------------------------
     returns 't' if the relation (person_id, on_what_table, on_which_id) exist
       in the table scs_person_rels
     returns 'f' if not
  */
  function exists_p (
    person_id     in scs_person_rels.person_id%TYPE,
    on_what_table in scs_person_rels.on_what_table%TYPE,
    on_which_id   in scs_person_rels.on_which_id%TYPE
  ) return char;
end scs_person_rel;
/
show errors



/* ======================================================================
   package bodies start here
====================================================================== */
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

/*
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
  end delete;
*/

  procedure delete_all (
    person_id     in scs_person_rels.person_id%TYPE
  )
  is
    tmp integer;
  begin
    delete scs_person_rels
     where person_id = scs_person_rel.delete_all.person_id;
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
    when DUP_VAL_ON_INDEX then
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
  exception
    when NO_DATA_FOUND then
      return null;
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
  exception
    when NO_DATA_FOUND then
      return null;
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
  exception
    when NO_DATA_FOUND then
      return null;
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
  exception
    when NO_DATA_FOUND then
      return null;
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
  exception
    when NO_DATA_FOUND then
      return null;
  end security_id;


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
  exception
    when NO_DATA_FOUND then
      return null;
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
