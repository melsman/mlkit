create table scs_roles(
  role_id integer
    constraint scs_roles_role_id_nn not null
    constraint scs_roles_role_id_pk primary key,
  abbreviation varchar2(30)
    constraint scs_roles_abbreviation_nn not null
    constraint scs_roles_abbreviation_un unique,
  role_description_tid integer
    constraint scs_roles_role_desc_tid_nn not null
    constraint scs_roles_role_desc_tid_fk references scs_texts(text_id)
);

create table scs_role_rels(
  role_id integer
    constraint scs_role_rels_role_id_nn not null
    constraint scs_role_rels_role_id_fk references scs_roles(role_id),
  party_id integer
    constraint scs_role_rels_party_id_nn not null
    constraint scs_role_rels_party_id_fk references scs_parties(party_id),
  constraint scs_role_rels_un unique (role_id,party_id)
);

/* 
======================================================================
package scs_role
======================================================================
*/
create or replace package scs_role 
is
  function new(
    object_id		   in scs_roles.role_id%TYPE default null,
    abbreviation	   in scs_roles.abbreviation%TYPE,
    role_description_tid   in scs_texts.text_id%TYPE
  ) return scs_roles.role_id%TYPE ;


  procedure destroy(
    role_id	 in scs_roles.role_id%TYPE default null,
    abbreviation in scs_roles.abbreviation%TYPE default null
  ) ;

  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  ) ;

  procedure remove(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE  
  ) ;

  function has_p(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  ) return char;

end scs_role;
/ 
show errors

create or replace package body scs_role
is
  function new(
    object_id		   in scs_roles.role_id%TYPE default null,
    abbreviation	   in scs_roles.abbreviation%TYPE,
    role_description_tid   in scs_texts.text_id%TYPE
  ) return scs_roles.role_id%TYPE
  is
    role_id scs_roles.role_id%TYPE;
  begin
    role_id := object_id;
    if role_id is null then
      role_id := scs.new_obj_id( object_id );
    end if;
    -- role_id is now a valid key

    insert into scs_roles( role_id, abbreviation, role_description_tid ) 
    values ( new.role_id, new.abbreviation, new.role_description_tid );

    return role_id;
  end new;


  procedure destroy(
    role_id	 in scs_roles.role_id%TYPE default null,
    abbreviation in scs_roles.abbreviation%TYPE default null
  )
  is
    v_role_id scs_roles.role_id%TYPE default null;
  begin
    v_role_id := destroy.role_id;
    if v_role_id is null then
      if destroy.abbreviation is null then
        return ; -- both arguments are null, do nothing
      else
        select role_id into v_role_id 
	from scs_roles
	where abbreviation = destroy.abbreviation; 
      end if;
    end if;
    -- now v_role_id is a valid key

    delete from scs_role_rels 
    where role_id = v_role_id;

    delete from scs_roles
    where role_id = v_role_id;

  end destroy;


  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  )
  is
  begin
    insert into scs_role_rels( party_id, role_id ) values ( add.party_id, add.role_id );
    commit;
  exception
    when dup_val_on_index then 
      commit; -- if party_id has the role already
  end add;


  procedure remove(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE  
  )
  is
  begin
    delete from scs_role_rels 
    where party_id = remove.party_id 
      and role_id = remove.role_id;
    commit;
  end remove;


  function has_p(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  ) return char
  is
    answer char;
    counter integer;
  begin
    answer := 'f';

    select count(*) into counter
    from scs_role_rels 
    where party_id = has_p.party_id
      and role_id = has_p.role_id;
    if counter=1 then
      answer := 't';
    end if;

    return answer;
  end has_p;

end scs_role;
/
show errors

