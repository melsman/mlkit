-- $Id$

-- This code i a down-sized and modified version of the party module
-- found in openACS (www.openacs.org): files groups-create.sql and
-- groups-body-create.sql

-- TO-DO
-- 2002-09-16, nh: who is allowed to approve group membership?


/* ======================================================================
   package scs_group

   provides functionality to create, destroy, ask for membership of groups

   History:
   021202 Kennie Nybo Pontoppidan <kennie@it-c.dk> added exception handling
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   161002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_group
is
  
  /* ------------
     function new
     ------------
     creates a new group

     throws a ScsDbExn exception if 
          there exists a group in scs_groups with this grp_id
       or 
          there exists a party in scs_parties with party_id 'grp_id'
       or 
          no group type exists in scs_grp_types with grp_type_id
       or 
          there exists a party in scs_parties new(with this email
       or 
          there exists a group with this grp_name
       or 
          join policy is not 'open', 'needs approval' or 'closed'
       or 
          no user exists with user_id 'modifying_user'
  */
  function new (
    grp_id         in scs_groups.grp_id%TYPE default null,
    grp_type_id    in scs_groups.grp_type_id%TYPE default null,
    email          in scs_parties.email%TYPE default null,
    url            in scs_parties.url%TYPE default null,
    grp_name       in scs_groups.grp_name%TYPE,
    join_policy    in scs_groups.join_policy%TYPE default null,
    modifying_user in scs_groups.modifying_user%TYPE
  ) return scs_groups.grp_id%TYPE;

  procedure destroy (
    grp_id in scs_groups.grp_id%TYPE
  );

  /* -------------
     function name
     -------------
     looks up grp_id in scs_groups and returns the group name (grp_name)
     
     returns null if grp_id is unknown in scs_groups
  */
  function name (
    grp_id in scs_groups.grp_id%TYPE
  ) return varchar2;


  /* --------------
     function getID
     --------------
     looks up name in scs_groups and returns the group ID
     
     returns null if name is unknown in scs_groups
  */
  function getID (
    name in scs_groups.grp_name%TYPE
  ) return scs_groups.grp_id%TYPE;

  /* -----------------
     function member_p
     -----------------
     returns 't' if party with id 'party_id' is member of group with id grp_id
             'f' if not

     the flag cascade_membership is 't' then the function checks for membership
     in super groups of the group as well

     never fails
  */
  function member_p (
    party_id in scs_parties.party_id%TYPE,
    grp_id   in scs_groups.grp_id%TYPE,
    cascade_membership char default 'f'
  ) return char;

  function check_representation (
    grp_id in scs_groups.grp_id%TYPE
  ) return char;

  function check_representation_all return char;
end scs_group;
/
show errors

/* ======================================================================
   package scs_grp_member_rel

   controls group membership

   History:
   061202 Kennie Nybo Pontoppidan <kennie@it-c.dk> added exception handling
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   161002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_grp_member_rel
as

  /* ------------
     function new
     ------------
     let A denote the group identified by 'grp_id'
     and x denote the party identified by 'party_id'

     creates a new membership relation: x is a member of A

     throws a ScsDbExn exception if
          there already exists a relation with rel_id 'rel_id'
       or
          grp_id is unknown or null
       or
          party_id is unknown or null
       or
          member_state is not 
	  'approved', 'needs approval', 'banned', 'rejected' or 'deleted'
       or
          modifying user is unknown or null
  */
  function new (
    rel_id              in scs_grp_member_rels.rel_id%TYPE default null,
    grp_id              in scs_grp_member_rels.grp_id%TYPE,
    party_id            in scs_grp_member_rels.party_id%TYPE,
    member_state        in scs_grp_member_rels.member_state%TYPE default 'approved',
    modifying_user      in scs_grp_member_rels.modifying_user%TYPE
  ) return scs_grp_member_rels.rel_id%TYPE;

  /* -----------------
     procedure destroy
     -----------------
     (physically) deletes a membership relation

     modifying user is currently not used

     never fails
  */
  procedure destroy (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE default null
  );

  /* -----------------
     procedure destroy
     -----------------
     (physically) deletes a membership relation

     modifying user is currently not used

     never fails
  */
  procedure destroy (
    grp_id         in scs_grp_member_rels.grp_id%TYPE,
    party_id       in scs_grp_member_rels.party_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE default null
  );

  /* -----------------------------
     function check_representation
     -----------------------------
     not documented, not tested yet...
  */
  function check_representation (
    rel_id in scs_grp_member_rels.rel_id%TYPE
  ) return char;


/* implemented, not used, not tested
  procedure ban (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  );
*/

/* implemented, not used, not tested
  procedure approve (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  );
*/

/* implemented, not used, not tested
  procedure reject (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  );
*/

/* implemented, not used, not tested
  procedure unapprove (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  );
*/

/* implemented, not used, not tested
  procedure deleted (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  );
*/

end scs_grp_member_rel;
/
show errors


/* ======================================================================
   package scs_grp_type


   History:
   011202 Kennie Nybo Pontoppidan <kennie@it-c.dk> added exception handling
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   161002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_grp_type
is
  /* ------------
     function new
     ------------
     creates a new group type

     throws a ScsDbExn if 
          there exists a group type with id 'grp_type_id'
       or 
          there exists a group type with name (grp_type) 'grp_type'
       or 
          default_join_policy is not 'open', 'needs approval' or 'closed'
       or 
          default_grp_type is not 't' or 'f'
       or 
          no user exists with user_id 'modifying_user'          
  */
  function new (
    grp_type_id         in scs_grp_types.grp_type_id%TYPE default null,
    grp_type            in scs_grp_types.grp_type%TYPE,
    default_join_policy in scs_grp_types.default_join_policy%TYPE default 'open',
    default_grp_type    in scs_grp_types.default_grp_type%TYPE default 'f',
    modifying_user      in scs_grp_types.modifying_user%TYPE
  ) return scs_grp_types.grp_type_id%TYPE;

  /* -------------------------
     function default_grp_type
     -------------------------
     looks up the id for the current default group type and returns it
     (there can be only one according to the 
     trigger scs_grp_types_def_grp_type_tr)

     throws a ScsDbExn if there is no default group type yet
  */
  function default_grp_type 
    return scs_grp_types.grp_type_id%TYPE;

-- this function is not currently used
--  procedure set_default_grp_type(
--    grp_type_id in scs_grp_types.grp_type_id%TYPE
--  );
     
end scs_grp_type;
/
show errors


/* ======================================================================
   package scs_grp_composition_rel

   

   History:
   051202 Kennie Nybo Pontoppidan <kennie@it-c.dk> added exception handling
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   161002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_grp_composition_rel
as
  /* ------------
     function new
     ------------
     creates a new composition: grp_one contains grp_two

     throws a ScsDbExn if
          rel_id exists in the table scs_grp_composition_rels
       or 
          one of grp_id_one, grp_id_two is unknown or null
       or 
          modifying user is unknown
       or 
          there is already a row with the pair (grp_id_one, grp_id_two)
	  (this excludes 2-cycles)
  */
  function new (
    rel_id         in scs_grp_composition_rels.rel_id%TYPE default null,
    grp_id_one     in scs_grp_composition_rels.grp_id_one%TYPE,
    grp_id_two     in scs_grp_composition_rels.grp_id_two%TYPE,
    modifying_user in scs_grp_composition_rels.modifying_user%TYPE
  ) return scs_grp_composition_rels.rel_id%TYPE;

  /* ----------------
     procedure destroy
     ----------------
     (physically) destroys a composition relation

     currently modifying_user is not used

     never fails
  */
  procedure destroy (
    rel_id	    in scs_grp_composition_rels.rel_id%TYPE,
    modifying_user  in scs_grp_composition_rels.modifying_user%TYPE
  );

  /* ----------------------------
     function check_path_exists_p
     ----------------------------
     let A denote the group with id 'component_id'
     and X denote the group with id 'container_id'

     returns 't' if 
          A = X
       or 
          there exists relations and groups
	  A -> B -> C -> ... -> X
	  (this means that A is a subgroup of X)
     returns 'f' if not

     never fails
  */
  function check_path_exists_p (
    component_id in scs_groups.grp_id%TYPE,
    container_id in scs_groups.grp_id%TYPE
  ) return char;

  /* -----------------------------
     function check_representation
     -----------------------------
     checks a composition relation for errors 

     returns 't' if there are no broken links in the denormalised table of 
		 composition- and member relations
     returns 'f' if
          there were broken links in the denormalised table of 
	  composition- and member relations (this is logged to scs_log)
       or
          rel_id was null or unknown

     never fails
  */
  function check_representation (
    rel_id in scs_grp_composition_rels.rel_id%TYPE
  ) return char;

  /* -------------------
     function contains_p
     -------------------
     let A denote the group with id 'grp_id'
     and x denote the component (in the view scs_grp_component_map) 
	 with id 'component_id'

     returns 't' if the component x belongs to the group A
	     't' if not

     if rel_id is specified, then only this relation is examined

     never fails
  */
  function contains_p (
    grp_id	 in scs_groups.grp_id%TYPE,
    component_id in scs_grp_component_map.component_id%TYPE,
    rel_id	 in scs_grp_component_map.rel_id%TYPE default null
  ) return char;

end scs_grp_composition_rel;
/
show errors




/* ======================================================================
   triggers are here
====================================================================== */

/* 
   -------------------------------------
   trigger scs_grp_types_def_grp_type_tr
   -------------------------------------
   checks that the number of default group types in scs_grp_types is 1

   throws a ScsDbExn exception if the number of default group types
   is different from 1 after an insert/update/delete
*/
create or replace trigger scs_grp_types_def_grp_type_tr
after insert or update or delete on scs_grp_types
declare 
  v_num_default_grp_type integer;
begin
  select count(*)
    into v_num_default_grp_type
    from scs_grp_types
   where default_grp_type = 't';

  if v_num_default_grp_type <> 1 then
    raise_application_error(scs.ScsDbExn,'wrong number of default group types');
  end if;
end scs_grp_types_def_grp_type_tr;
/
show errors

/* 
   ---------------------------------
   trigger scs_grp_member_rels_in_tr
   ---------------------------------
*/
create or replace trigger scs_grp_member_rels_in_tr
after insert on scs_grp_member_rels
for each row
begin
  -- Insert a row in the scs_grp_party_index
  insert into scs_grp_party_index
   (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
  values
   (:new.grp_id, :new.party_id, :new.rel_id, :new.grp_id, 'membership_rel');

  -- For all groups of which I am a component, insert a
  -- row in the scs_grp_party_index.
  for map in (select distinct grp_id
                from scs_grp_component_map
               where component_id = :new.grp_id) loop
    insert into scs_grp_party_index
      (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
    values
      (map.grp_id, :new.party_id, :new.rel_id, :new.grp_id, 'membership_rel');
  end loop;
end;
/
show errors

/* 
   ----------------------------------
   trigger scs_grp_member_rels_del_tr
   ----------------------------------
*/
create or replace trigger scs_grp_member_rels_del_tr
before delete on scs_grp_member_rels
for each row
begin
  delete from scs_grp_party_index
   where rel_id = :old.rel_id;
end;
/
show errors;

/* 
   --------------------------------------
   trigger scs_grp_composition_rels_in_tr
   --------------------------------------
*/
create or replace trigger scs_grp_composition_rels_in_tr
after insert on scs_grp_composition_rels
for each row
begin

  -- Insert a row for me in group_element_index
  insert into scs_grp_party_index
    (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
  values
    (:new.grp_id_one, :new.grp_id_two, :new.rel_id, :new.grp_id_one, 'composition_rel');

  -- Make my elements be elements of my new composite group
  -- The last "and rel_id = m.rel_id IS necessary because the same
  -- member_id can be in the same group several times comming from
  -- different groups due to group composition. 2002-09-18, nh
  -- TO-DO: make a test case that shows this fact.
  insert into scs_grp_party_index
    (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
  select distinct
    :new.grp_id_one, element_id, rel_id, container_id, ancestor_rel_type
    from scs_grp_element_map m
   where grp_id = :new.grp_id_two
     and not exists (select 1
                       from scs_grp_element_map
                      where grp_id = :new.grp_id_one
                        and element_id = m.element_id
                        and rel_id = m.rel_id);

  -- For all direct or indirect containers of my new composite group, 
  -- add me and add my elements
  for map in (select distinct grp_id
	      from scs_grp_component_map
	      where component_id = :new.grp_id_one) loop

    -- Add a row for me
    -- This is interesting and a litle surprise. why is it resonable
    -- to include this composition relation - it is indirect?
    -- 2002-09-18, nh
    insert into scs_grp_party_index
      (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
    values
      (map.grp_id, :new.grp_id_two, :new.rel_id, :new.grp_id_one, 'composition_rel');

    -- Add rows for my elements
    insert into scs_grp_party_index
       (grp_id, party_id, rel_id, container_id, ancestor_rel_type)
    select distinct
        map.grp_id, element_id, rel_id, container_id, ancestor_rel_type
     from scs_grp_element_map m
    where grp_id = :new.grp_id_two
      and not exists (select 1
		        from scs_grp_element_map
                       where grp_id = map.grp_id
                         and element_id = m.element_id
                         and rel_id = m.rel_id);
  end loop;
end; -- ends trigger scs_grp_composition_rels_in_tr
/
show errors

/* 
   --------------------------------
   trigger scs_grp_comp_rels_del_tr
   --------------------------------
*/
create or replace trigger scs_grp_comp_rels_del_tr
before delete on scs_grp_composition_rels
for each row
declare
  n_rows integer;
begin
  for map in (select *
	        from scs_grp_component_map
               where rel_id = :old.rel_id) loop

    delete from scs_grp_party_index
     where rel_id = :old.rel_id;

    select count(*) 
      into n_rows
      from scs_grp_component_map
     where grp_id = map.grp_id
       and component_id = map.component_id;

    if n_rows = 0 then
      delete from scs_grp_party_index
       where grp_id = map.grp_id
         and container_id = map.component_id
         and ancestor_rel_type = 'membership_rel';
    end if;

  end loop;

  for map in (select *
                from scs_grp_component_map
	       where grp_id in (select grp_id
		                  from scs_grp_component_map
		                 where component_id = :old.grp_id_one
			         union
			        select :old.grp_id_one
			          from dual)
                 and component_id in (select component_id
			                from scs_grp_component_map
			               where grp_id = :old.grp_id_two
				       union
				      select :old.grp_id_two
				        from dual)
                 and scs_grp_composition_rel.contains_p(grp_id, component_id, rel_id) = 'f') loop

    delete from scs_grp_party_index
     where grp_id = map.grp_id
       and party_id = map.component_id
       and rel_id = map.rel_id;

    select count(*) 
      into n_rows
      from scs_grp_component_map
     where grp_id = map.grp_id
       and component_id = map.component_id;

    if n_rows = 0 then
      delete from scs_grp_party_index
       where grp_id = map.grp_id
         and container_id = map.component_id
         and ancestor_rel_type = 'membership_rel';
    end if;

  end loop;
end;
/
show errors


/* ======================================================================
   package bodies start here 
====================================================================== */

create or replace package body scs_grp_type
is
  function new (
    grp_type_id         in scs_grp_types.grp_type_id%TYPE default null,
    grp_type            in scs_grp_types.grp_type%TYPE,
    default_join_policy in scs_grp_types.default_join_policy%TYPE default 'open',
    default_grp_type    in scs_grp_types.default_grp_type%TYPE default 'f',
    modifying_user      in scs_grp_types.modifying_user%TYPE
  ) return scs_grp_types.grp_type_id%TYPE
  is
    v_grp_type_id scs_grp_types.grp_type_id%TYPE;
    errorText	  varchar2(100);
  begin
    v_grp_type_id := scs.new_obj_id(new.grp_type_id);

    insert into scs_grp_types(
      grp_type_id, 
      grp_type, 
      default_join_policy, 
      default_grp_type, 
      modifying_user )
    values (
      v_grp_type_id, 
      grp_type, 
      default_join_policy, 
      default_grp_type, 
      modifying_user );

    return v_grp_type_id;
  exception
    when others then
      raise_application_error( scs.ScsDbExn, 'scs_grp_type.new: ' || SQLERRM );
  end new;

  function default_grp_type return scs_grp_types.grp_type_id%TYPE
  is
    v_grp_type_id scs_grp_types.grp_type_id%TYPE;
  begin
    select grp_type_id
      into v_grp_type_id
      from scs_grp_types
     where scs_grp_types.default_grp_type = 't';

    return v_grp_type_id;
  exception
    when NO_DATA_FOUND then
      raise_application_error( scs.ScsDbExn, 'scs_grp_type.default_grp_type: ' 
					     || SQLERRM );
  end default_grp_type;

  procedure set_default_grp_type(
    grp_type_id in scs_grp_types.grp_type_id%TYPE
  )
  is
    v_old_grp_type_id scs_grp_types.grp_type_id%TYPE;
  begin
    select grp_type_id
      into v_old_grp_type_id
      from scs_grp_types
     where default_grp_type = 't';

    if v_old_grp_type_id = set_default_grp_type.grp_type_id then
      return;
    end if;

    update scs_grp_types
       set default_grp_type = decode(grp_type_id,set_default_grp_type.grp_type_id,'t','f')
     where grp_type_id in (v_old_grp_type_id, set_default_grp_type.grp_type_id);
  end set_default_grp_type;
end scs_grp_type;
/
show errors


------------------------------------------------------
create or replace package body scs_grp_composition_rel
as
  function new (
    rel_id         in scs_grp_composition_rels.rel_id%TYPE default null,
    grp_id_one     in scs_grp_composition_rels.grp_id_one%TYPE,
    grp_id_two     in scs_grp_composition_rels.grp_id_two%TYPE,
    modifying_user in scs_grp_composition_rels.modifying_user%TYPE
  ) return scs_grp_composition_rels.rel_id%TYPE
  is
    v_rel_id scs_grp_composition_rels.rel_id%TYPE;
  begin
    v_rel_id := scs.new_obj_id(new.rel_id);

    insert into scs_grp_composition_rels
      (rel_id, grp_id_one, grp_id_two, modifying_user)
    values
      (v_rel_id, grp_id_one, grp_id_two, modifying_user);    

    return v_rel_id;
  exception
    when others then
      raise_application_error( scs.ScsDbExn, 'scs_grp_composition_rel.new: ' 
					     || SQLERRM );
  end;

  -- TO-DO: auditing
  procedure destroy (
    rel_id         in scs_grp_composition_rels.rel_id%TYPE,
    modifying_user in scs_grp_composition_rels.modifying_user%TYPE
  )
  is
  begin
    delete from scs_grp_composition_rels
     where rel_id = scs_grp_composition_rel.destroy.rel_id;
  end destroy;

  function check_path_exists_p (
    component_id in scs_groups.grp_id%TYPE,
    container_id in scs_groups.grp_id%TYPE
  ) return char
  is
  begin
    if component_id = container_id then
      return 't';
    end if;

    for row in (select grp_id_one as parent_id
                  from scs_grp_composition_rels
                where grp_id_two = check_path_exists_p.component_id) loop
      if check_path_exists_p(row.parent_id, check_path_exists_p.container_id) = 't' then
        return 't';
      end if;
    end loop;
    
    return 'f';
  end;

  function check_index (
    component_id in scs_groups.grp_id%TYPE,
    container_id in scs_groups.grp_id%TYPE
  ) return char
  is
    result char(1);
    n_rows integer;
  begin
    result := 't';

    -- Loop through all the direct containers (DC) of COMPONENT_ID
    -- that are also contained by CONTAINER_ID and verify that the
    -- SCS_GRP_COMPONENT_MAP contains the (CONTAINER_ID, DC.REL_ID,
    -- COMPONENT_ID) triple.
    for dc in (select rel_id, grp_id_one as container_id
                 from scs_grp_composition_rels
                where grp_id_two = check_index.component_id) loop

      if check_path_exists_p(dc.container_id,
                             check_index.container_id) = 't' then
        -- The triple (check_index.container_id,dc.rel_id,check_index.component_id) must exists.
        select decode(count(*),0,0,1) 
          into n_rows
          from scs_grp_component_map
         where grp_id = check_index.container_id
           and component_id = check_index.component_id
           and rel_id = dc.rel_id;

        if n_rows = 0 then
          result := 'f';
          scs_log.error('scs_grp_composition_rel.check_index',
                        'Row missing from scs_grp_component_map for (' ||
                        'group_id = ' || container_id || '(' || container_id || '), ' ||
                        'component_id = ' || component_id || '(' || component_id || '), ' ||
                        'rel_id = ' || dc.rel_id || ')');
        end if;
      end if;

    end loop;

    -- Loop through all the containers of CONTAINER_ID.
    for r1 in (select grp_id_one as container_id
                 from scs_grp_composition_rels
                where grp_id_two = check_index.container_id
                union
               select check_index.container_id
                 from dual) loop
      -- Loop through all the components of COMPONENT_ID and make a
      -- recursive call.
      for r2 in (select grp_id_two as component_id
                   from scs_grp_composition_rels
                  where grp_id_one = check_index.component_id
                  union
                 select check_index.component_id
                   from dual) loop
        if (r1.container_id != check_index.container_id or
            r2.component_id != check_index.component_id) and
            check_index(r2.component_id, r1.container_id) = 'f' then
          result := 'f';
        end if;
      end loop;
    end loop;

    return result;
  end;

  function check_representation (
    rel_id in scs_grp_composition_rels.rel_id%TYPE
  ) return char
  is
    v_container_id scs_groups.grp_id%TYPE;
    v_component_id scs_groups.grp_id%TYPE;
    result char(1);
  begin
    result := 't';

    select grp_id_one, grp_id_two
      into v_container_id, v_component_id
      from scs_grp_composition_rels
     where rel_id = check_representation.rel_id;

    -- First let's check that the index has all the rows it should.
    if check_index(v_component_id, v_container_id) = 'f' then
      result := 'f';
    end if;

    -- Now let's check that the index doesn't have any extraneous rows
    -- relating to this relation.
    for row in (select *
                  from scs_grp_component_map
                 where rel_id = check_representation.rel_id) loop
      if check_path_exists_p(row.component_id, row.grp_id) = 'f' then
        result := 'f';
        scs_log.error('scs_grp_composition_rel.check_representation',
                      'Extraneous row in group_component_index: ' ||
                      'group_id = ' || row.grp_id || ', ' ||
                      'component_id = ' || row.component_id || ', ' ||
                      'rel_id = ' || row.rel_id || ', ' ||
                      'container_id = ' || row.container_id || '.');
      end if;
    end loop;

    return result;
  exception 
    when NO_DATA_FOUND then
      return 'f';
  end;

  function contains_p (
    grp_id	 in scs_groups.grp_id%TYPE,
    component_id in scs_grp_component_map.component_id%TYPE,
    rel_id	 in scs_grp_component_map.rel_id%TYPE default null
  ) return char
  is
  begin
    if grp_id = component_id then
      return 't';
    else
      if rel_id is null then
        for map in (select *
                      from scs_grp_component_map
                   where component_id = contains_p.component_id
                     and grp_id = container_id) loop
          if contains_p(grp_id, map.grp_id) = 't' then
            return 't';
          end if;
        end loop;
      else
        for map in (select *
                      from scs_grp_component_map
                     where component_id = contains_p.component_id
                       and rel_id = contains_p.rel_id
                       and grp_id = container_id) loop
          if contains_p(grp_id, map.grp_id) = 't' then
            return 't';
          end if;
        end loop;
      end if;
      
      return 'f';
    end if;
  end contains_p;

end scs_grp_composition_rel;
/
show errors


-------------------------------------------------
create or replace package body scs_grp_member_rel
as
  function new (
    rel_id              in scs_grp_member_rels.rel_id%TYPE default null,
    grp_id              in scs_grp_member_rels.grp_id%TYPE,
    party_id            in scs_grp_member_rels.party_id%TYPE,
    member_state        in scs_grp_member_rels.member_state%TYPE default 'approved',
    modifying_user      in scs_grp_member_rels.modifying_user%TYPE
  ) return scs_grp_member_rels.rel_id%TYPE
  is
    v_rel_id scs_grp_member_rels.rel_id%TYPE;
  begin
    v_rel_id := scs.new_obj_id(new.rel_id);

    insert into scs_grp_member_rels
      (rel_id, grp_id, party_id, member_state, modifying_user)
    values
      (v_rel_id, new.grp_id, new.party_id, new.member_state, new.modifying_user);

    return v_rel_id;
  exception 
    when others then
      raise_application_error( scs.ScsDbExn, 
			       'scs_grp_member_rel.new: ' || SQLERRM );
  end new;

/*
  -- TO-DO we have to think about auditing! 2002-09-18, nh
  procedure ban (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    update scs_grp_member_rels
       set member_state = 'banned',
           modifying_user = ban.modifying_user,
           last_modified = sysdate
     where rel_id = ban.rel_id;
  end ban;
*/

/*
  procedure approve (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    update scs_grp_member_rels
       set member_state = 'approved',
           modifying_user = approve.modifying_user,
           last_modified = sysdate
     where rel_id = approve.rel_id;
   end approve;
*/

/*
  procedure reject (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    update scs_grp_member_rels
       set member_state = 'rejected',
           modifying_user = reject.modifying_user,
           last_modified = sysdate
     where rel_id = reject.rel_id;
  end reject;
*/

/*
  procedure unapprove (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    update scs_grp_member_rels
       set member_state = 'needs approval',
           modifying_user = unapprove.modifying_user,
           last_modified = sysdate
     where rel_id = unapprove.rel_id;
  end unapprove;
*/
/*
  procedure deleted (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    update scs_grp_member_rels
       set member_state = 'deleted',
           modifying_user = deleted.modifying_user,
           last_modified = sysdate
     where rel_id = deleted.rel_id;
  end deleted;
*/
  procedure destroy (
    rel_id         in scs_grp_member_rels.rel_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    delete scs_grp_member_rels
    where rel_id = destroy.rel_id;
  end destroy;

  procedure destroy (
    grp_id         in scs_grp_member_rels.grp_id%TYPE,
    party_id       in scs_grp_member_rels.party_id%TYPE,
    modifying_user in scs_grp_member_rels.modifying_user%TYPE    
  )
  is
  begin
    delete from scs_grp_member_rels
     where grp_id = scs_grp_member_rel.destroy.grp_id
       and party_id = destroy.party_id;
  end destroy;

  function check_index (
    grp_id       in scs_groups.grp_id%TYPE,
    member_id    in scs_parties.party_id%TYPE,
    container_id in scs_groups.grp_id%TYPE
  ) return char
  is
    result char(1);
    n_rows integer;
  begin
    result := 't';

    select count(*) 
      into n_rows
      from scs_grp_member_map
     where grp_id = check_index.grp_id
       and member_id = check_index.member_id
       and container_id = check_index.container_id;

    if n_rows = 0 then
      result := 'f';
      scs_log.error('scs_grp_member_rel.check_index',
                    'Row missing from scs_grp_member_map: ' ||
                    'grp_id = ' || check_index.grp_id || '(' || check_index.grp_id || '), ' ||
                    'member_id = ' || check_index.member_id || '(' || scs_party.email(check_index.member_id) || '), ' ||
                    'container_id = ' || check_index.container_id || '(' || container_id || ').');
    end if;

    for row in (select grp_id_one as container_id
                  from scs_grp_composition_rels c
                where c.grp_id_two = check_index.grp_id) loop
      if check_index(row.container_id, check_index.member_id, check_index.container_id) = 'f' then
        result := 'f';
      end if;
    end loop;

    return result;
  end;

  function check_representation (
    rel_id in scs_grp_member_rels.rel_id%TYPE
  ) return char
  is
    v_grp_id scs_groups.grp_id%TYPE;
    v_member_id scs_parties.party_id%TYPE;
    result char(1);
  begin
    result := 't';

    select grp_id, party_id
      into v_grp_id, v_member_id
      from scs_grp_member_rels
     where rel_id = check_representation.rel_id;

    -- member_id must be a direct member of grp_id
    if check_index(v_grp_id, v_member_id, v_grp_id) = 'f' then
      result := 'f';
    end if;

    for row in (select *
                  from scs_grp_member_map
                 where rel_id = check_representation.rel_id) loop
      if scs_grp_composition_rel.check_path_exists_p(row.container_id,
                                                     row.grp_id) = 'f' then
        result := 'f';
        scs_log.error('scs_grp_member_rel.check_representation',
                      'Extra row in scs_grp_member_map: ' ||
                      'group_id = ' || row.grp_id || ', ' ||
                      'member_id = ' || row.member_id || ', ' ||
                      'container_id = ' || row.container_id || '.');
      end if;
    end loop;

    return result;
  exception
    when NO_DATA_FOUND then
      return 'f';
  end;

end scs_grp_member_rel;
/
show errors


----------------------------------------
create or replace package body scs_group
is
  function new (
    grp_id         in scs_groups.grp_id%TYPE default null,
    grp_type_id    in scs_groups.grp_type_id%TYPE default null,
    email          in scs_parties.email%TYPE default null,
    url            in scs_parties.url%TYPE default null,
    grp_name       in scs_groups.grp_name%TYPE,
    join_policy    in scs_groups.join_policy%TYPE default null,
    modifying_user in scs_groups.modifying_user%TYPE
  ) return scs_groups.grp_id%TYPE
  is
    v_grp_id scs_groups.grp_id%TYPE;
    v_join_policy scs_groups.join_policy%TYPE;
    v_grp_type_id scs_grp_types.grp_type_id%TYPE;
  begin
    v_grp_id := scs_party.new(party_id => grp_id, 
                              party_type => 'scs_groups',
                              email => email,
                              url => url,
                              modifying_user => modifying_user);
    v_join_policy := join_policy;

    -- if group type wasn't specified, select the default.    
    if grp_type_id is null then
      v_grp_type_id := scs_grp_type.default_grp_type();
    else 
      v_grp_type_id := grp_type_id;
    end if;

    -- if join policy wasn't specified, select the default based on group type
    if v_join_policy is null then
      select default_join_policy 
        into v_join_policy
        from scs_grp_types
       where grp_type_id = v_grp_type_id;
--    else
--      v_join_policy := 'open';
    end if;

    insert into scs_groups
      (grp_id, grp_type_id, grp_name, join_policy, last_modified, modifying_user)
    values (v_grp_id, v_grp_type_id, grp_name, v_join_policy, sysdate, modifying_user);

    return v_grp_id;
  exception 
    when others then
      raise_application_error( scs.ScsDbExn, 'scs_group.new:' || SQLERRM );
  end new;

  -- TO_DO: modifying user, and then auditing info - if we want that info? 2002-09-17, nh
  procedure destroy (
    grp_id in scs_groups.grp_id%TYPE
  )
  is
  begin
    -- Delete all member relations.
    delete from scs_grp_member_rels
     where grp_id = scs_group.destroy.grp_id;

    -- Delete all composition relations.
    delete from scs_grp_composition_rels
     where grp_id_one = scs_group.destroy.grp_id
        or grp_id_two = scs_group.destroy.grp_id; 

    delete from scs_groups
     where grp_id = scs_group.destroy.grp_id;

    scs_party.destroy(scs_group.destroy.grp_id);
  end destroy;

  function getID (
    name in scs_groups.grp_name%TYPE
  ) return scs_groups.grp_id%TYPE
  is
    grp_id scs_groups.grp_id%TYPE;
  begin
    select grp_id 
    into getID.grp_id
    from scs_groups
    where grp_name = getID.name;

    return grp_id;
  exception
    when NO_DATA_FOUND then
      return null;
  end getID;

  function name (
    grp_id in scs_groups.grp_id%TYPE
  ) return varchar2
  is
    v_grp_name varchar2(200);
  begin
    select grp_name
      into v_grp_name
      from scs_groups
     where grp_id = scs_group.name.grp_id;

    return v_grp_name;
  exception
    when NO_DATA_FOUND then
      return null;
  end name;


  function member_p (
    party_id in scs_parties.party_id%TYPE,
    grp_id   in scs_groups.grp_id%TYPE,
    cascade_membership char
  ) return char
  is
    m_result integer;
  begin
    if cascade_membership = 't' then
      select count(*)
        into m_result
        from scs_grp_approved_member_map
       where grp_id = member_p.grp_id 
         and member_id = member_p.party_id;

      if m_result > 0 then
        return 't';
      end if;
    else
      select count(*)
        into m_result
        from scs_grp_member_rels rels
       where rels.grp_id = member_p.grp_id
         and rels.party_id = member_p.party_id
         and rels.member_state = 'approved';

      if m_result > 0 then
        return 't';
      end if;
    end if;

    return 'f';
  end member_p;

  function check_representation (
   grp_id in scs_groups.grp_id%TYPE
  ) return char
  is
    result char(1);
  begin
    result := 't';

    scs_log.notice('scs_group.check_representation',
                   'Running check_representation on group ' || grp_id);

    for c in (select rel_id
                from scs_grp_composition_rels
               where grp_id_one = grp_id) loop
      if scs_grp_composition_rel.check_representation(c.rel_id) = 'f' then
        result := 'f';
      end if;
    end loop;

    for m in (select rel_id
              from scs_grp_member_rels
              where grp_id = grp_id) loop
      if scs_grp_member_rel.check_representation(m.rel_id) = 'f' then
        result := 'f';
      end if;
    end loop;

    scs_log.notice('scs_group.check_representation',
                   'Done running check_representation on group ' || grp_id);
    return result;
  end;

  function check_representation_all
  return char
  is
    v_res char(1);
  begin
    v_res := 't';
    for g in (select * 
                from scs_groups) loop
      if scs_group.check_representation(g.grp_id) = 'f' then
        v_res := 'f';
        scs_log.error('scs_group.check_representation_all',
                      'Group ' || g.grp_name || ' (' || g.grp_id || ') failed.');
      end if;
    end loop;
  
    return v_res;
  end check_representation_all;

end scs_group;
/
show errors





