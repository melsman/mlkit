-- $Id$

/* ======================================================================
   package scs_role

   provides functionaily for creating and destroying (physically deleting) 
   roles, assigning/removing roles to/from parties and query functions:
   has this party this role?

   History:
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   300902 Niels Hallenberg <nh@it.edu> created package
======================================================================*/
create or replace package scs_role 
is
  /* ------------
     function new
     ------------
     creates a new role and returns the role_id for the new role

     if object_id is null, a new id is created for the role

     throws a ScsDbExn if 
          there exists a role with the provided object_id
       or 
          there exists a role with the provided abbreviation
       or 
          abbreviation was null
       or 
	  role_description_tid was null or unknown
  */
  function new(
    object_id		   in scs_roles.role_id%TYPE default null,
    abbreviation	   in scs_roles.abbreviation%TYPE,
    role_description_tid   in scs_texts.text_id%TYPE
  ) return scs_roles.role_id%TYPE ;

  /* -----------------
     procedure destroy
     -----------------
     destroys a role and all relations with this role

     the procedure can be called with role_id or abbreviation 
     as role the identifier. If role_id is null, the role_id will 
     be looked up using abbreviation

     never fails
  */  
  procedure destroy(
    role_id	 in scs_roles.role_id%TYPE default null,
    abbreviation in scs_roles.abbreviation%TYPE default null
  );

  /* -------------------------
     function abbrev_to_roleid
     -------------------------
     looks up the abbreviation in scs_roles 
     and returns the corresponding role_id

     throws a ScsDbExn exception if 'role_abbrev' is an unknown abbreviation
  */
  function abbrev_to_roleid(
    role_abbrev in scs_roles.abbreviation%TYPE
  ) return scs_roles.role_id%TYPE;

  /* -------------
     procedure add
     -------------
     assigns the role with role_id 'role_id' 
     to the party with party_id 'party_id'

     throws a ScsDbExn exception if party_id or role_id are unknown
  */
  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  );

  /* -------------
     procedure add
     -------------
     assigns the role with abbreviation 'role_abbrev' 
     to the party with party_id 'party_id'

     throws a ScsDbExn exception if party_id or role_abbrev are unknown
  */
  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  );

  /* ----------------
     procedure remove
     ----------------
     removes the role with role_id 'role_id' from the party with 'party_id'

     never fails
  */
  procedure remove(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE  
  );

  /* ----------------
     procedure remove
     ----------------
     removes the role with abbreviation 'role_abbrev' 
     from the party with 'party_id'

     never fails
  */
  procedure remove(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  );

  /* --------------
     function has_p
     --------------
     looks up the pair (party_id, role_id) in scs_role_rels

     returns 't' if party with 'party_id' has role with 'role_id'
	     'f' else

     never fails
  */
  function has_p(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  ) return char;

  /* --------------
     function has_p
     --------------
     looks up the pair (party_id, role_id) in scs_role_rels,
       where role_id is associated with 'role_abbrev'

     returns 't' if party with 'party_id' has role with 'role_id'
	     'f' else

     never fails
  */
  function has_p(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  ) return char;

end scs_role;
/ 
show errors


/* ======================================================================
   package bodies start here
====================================================================== */
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
  exception
    when others then
      raise_application_error( scs.ScsDbExn, 
			       'scs_role.new: cannot create new role,'
			       || SQLERRM );
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
  exception
    when others then
      return; -- we silently ignore that the role does not exists.
  end destroy;

  function abbrev_to_roleid(
    role_abbrev in scs_roles.abbreviation%TYPE
  ) return scs_roles.role_id%TYPE
  is 
    v_role_id scs_roles.role_id%TYPE;
  begin
    select role_id
      into v_role_id
      from scs_roles
     where abbreviation = abbrev_to_roleid.role_abbrev;
    return v_role_id;
  exception
    when NO_DATA_FOUND then
      raise_application_error( scs.ScsDbExn,
			       'scs_role.abbrev_to_roleid: role ' || 
			       role_abbrev || ' does not exists');
  end abbrev_to_roleid;

  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_id	in scs_roles.role_id%TYPE
  )
  is
  begin
    insert into scs_role_rels( party_id, role_id ) values ( add.party_id, add.role_id );
  exception
    when dup_val_on_index then 
      return; -- if party_id has the role already
    when others then
      raise_application_error(scs.ScsDbExn,'scs_role.add: role_id ' || role_id || ' or ' ||
                                           'party_id ' || party_id || ' does not exists');
  end add;

  procedure add(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  )
  is
  begin
    scs_role.add(party_id => party_id,
                 role_id => scs_role.abbrev_to_roleid(add.role_abbrev));
  exception
    when others then
      raise_application_error(scs.ScsDbExn,'scs_role.add: role ' || role_abbrev || ' or ' ||
                                           'party_id ' || party_id || ' does not exists');
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
  end remove;

  procedure remove(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  ) 
  is
  begin
    scs_role.remove(party_id => remove.party_id,
                    role_id => scs_role.abbrev_to_roleid(remove.role_abbrev));
  exception
    when others then
      return; -- Ignore, that role or party_id does not exist.
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

    select count(*) 
      into counter
      from scs_role_rels 
    where role_id = has_p.role_id
      and (party_id = has_p.party_id or
           party_id in (select grp_id
                          from scs_grp_approved_member_map
                         where scs_grp_approved_member_map.member_id = has_p.party_id));

    if counter > 0 then
      answer := 't';
    end if;

    return answer;
  end has_p;

  function has_p(
    party_id	in scs_parties.party_id%TYPE,
    role_abbrev in scs_roles.abbreviation%TYPE
  ) return char
  is
  begin
    return has_p(party_id => party_id,
                 role_id => scs_role.abbrev_to_roleid(has_p.role_abbrev));
  exception
    when others then
      return 'f'; -- Role does not exists.
  end has_p;

end scs_role;
/
show errors

