-- $Id$

/* ======================================================================
   package scs_priority

   History:
   2003-12-29 Kennie Nybo Pontoppidan <kennie@it-c.dk> created package
====================================================================== */
create or replace package scs_priority
as
  /* ------------
     function new
     ------------
     creates a new priority relation 
     returns the rel_id for the created row

     throws a ScsDbExn exception if table constraints are violated
  */
  function new(
    on_what_parent_table in scs_priority_rels.on_what_parent_table%TYPE,
    on_which_parent_id	 in scs_priority_rels.on_which_parent_id%TYPE,
    on_what_child_table	 in scs_priority_rels.on_what_child_table%TYPE,
    on_which_child_id	 in scs_priority_rels.on_which_parent_id%TYPE,
    modified_by		 in integer
  ) return scs_priority_rels.rel_id%TYPE;


  /* ------------------------
     procedure swapPriorities
     ------------------------
     swaps the priorities for two rows in scs_priority_rels

     throws a ScsDbExn if
          there is an unknown rel_id
       or 
          rel_ids are associated with different (parent,child) pairs
  */  
  procedure swapPriorities(
    rel_id1	in scs_priority_rels.rel_id%TYPE,
    rel_id2	in scs_priority_rels.rel_id%TYPE
  ) ;

  procedure increasePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  ) ;

  procedure decreasePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  ) ;

  /* ------------------------
     function getTruePriority
     ------------------------
     calculates the true priority of the priority_rel 
     (when a row with priority = 1 is deleted in scs_priority_rels, the row
      with priority = 2 is really the first priority )

     never fails
  */  
  function getTruePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  ) return integer;
end scs_priority;
/
show errors


/* ======================================================================
   package bodies start here
====================================================================== */ 
create or replace package body scs_priority
as

  function new(
    on_what_parent_table in scs_priority_rels.on_what_parent_table%TYPE,
    on_which_parent_id	 in scs_priority_rels.on_which_parent_id%TYPE,
    on_what_child_table	 in scs_priority_rels.on_what_child_table%TYPE,
    on_which_child_id	 in scs_priority_rels.on_which_parent_id%TYPE,
    modified_by		 in integer
  ) return scs_priority_rels.rel_id%TYPE
  is
    rel_id        scs_priority_rels.rel_id%TYPE;
    next_priority scs_priority_rels.priority%TYPE;
  begin
    select count (*) into next_priority
      from scs_priority_rels
     where on_what_parent_table = new.on_what_parent_table
       and on_which_parent_id	= new.on_which_parent_id
       and on_what_child_table  = new.on_what_child_table;
    next_priority := next_priority + 1;

    rel_id := scs.new_obj_id;

    insert into scs_priority_rels(
      REL_ID 		     ,
      ON_WHAT_PARENT_TABLE   ,
      ON_WHICH_PARENT_ID     ,
      ON_WHAT_CHILD_TABLE    ,
      ON_WHICH_CHILD_ID	     ,
      PRIORITY		     ,
      MODIFYING_USER 	
    ) values (
      rel_id,
      new.on_what_parent_table ,
      new.on_which_parent_id   ,
      new.on_what_child_table  ,
      new.on_which_child_id    ,
      next_priority	       ,
      new.modified_by    
    );

    return rel_id;
  exception
    when others then 	      
      raise_application_error(
        scs.ScsDbExn, 'scs_priority.new: unknown error' );
  end new; 


  procedure swapPriorities(
    rel_id1	in scs_priority_rels.rel_id%TYPE,
    rel_id2	in scs_priority_rels.rel_id%TYPE
  )
  is 
    on_what_parent_table1	scs_priority_rels.on_what_parent_table%TYPE;
    on_which_parent_id1		scs_priority_rels.on_which_parent_id%TYPE;
    on_what_child_table1	scs_priority_rels.on_what_child_table%TYPE;
    priority1			integer;
    on_what_parent_table2	scs_priority_rels.on_what_parent_table%TYPE;
    on_which_parent_id2		scs_priority_rels.on_which_parent_id%TYPE;
    on_what_child_table2	scs_priority_rels.on_what_child_table%TYPE;
    priority2			integer;
    not_in_same_parent_child_rel	exception;
  begin
    select on_what_parent_table, on_which_parent_id, on_what_child_table,
	   priority 
      into on_what_parent_table1, on_which_parent_id1, on_what_child_table1, 
	   priority1 
      from scs_priority_rels 
     where rel_id = rel_id1;

    select on_what_parent_table, on_which_parent_id, on_what_child_table,
	   priority
      into on_what_parent_table2, on_which_parent_id2, on_what_child_table2,
	   priority2 
      from scs_priority_rels 
     where rel_id = rel_id2;

    if ( on_what_parent_table1 <> on_what_parent_table2 ) or
       ( on_which_parent_id1 <> on_which_parent_id2 ) or
       ( on_what_child_table1 <> on_what_child_table2 ) then
      raise not_in_same_parent_child_rel;
    end if;
    
    -- NB (knp) this is the nice way to do it: 
    --   update scs_priority_rels
    --   set priority = 
    --     case when rel_id = swapPriorities.rel_id1 then swapPriorities.priority2
    --          when rel_id = swapPriorities.rel_id2 then swapPriorities.priority1
    --     end 
    --   where rel_id in (rel_id1, rel_id2);
    --
    -- Unfortunately it only works from sqlplus (without variables) 
    -- and cannot be parsed as a procedure by the plsql parser:
    update scs_priority_rels
    set priority = -1
    where rel_id = rel_id1;

    update scs_priority_rels
    set priority = priority1
    where rel_id = rel_id2;

    update scs_priority_rels
    set priority = priority2
    where rel_id = rel_id1;
  exception
    when NO_DATA_FOUND then
      raise_application_error( 
        scs.ScsDbExn, 'scs_priority.swapPriorities: illegal rel_ids ' 
		      || rel_id1 || ',' || rel_id2 );
    when not_in_same_parent_child_rel then
      raise_application_error( 
        scs.ScsDbExn, 'scs_priority.swapPriorities: rel_ids ' || rel_id1 || 
		      ',' || rel_id2 || 'must belong to same the enumeration');
  end swapPriorities;


  procedure increasePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  )
  is
    priority1			integer;
    on_what_parent_table1	varchar2(100);
    on_which_parent_id1		integer;
    on_what_child_table1	varchar2(100);
    priority2			integer;
    rel_id2			integer;
  begin
    select on_what_parent_table, on_which_parent_id, 
	   on_what_child_table, priority 
      into on_what_parent_table1, on_which_parent_id1, 
	   on_what_child_table1, priority1 
      from scs_priority_rels
     where rel_id = increasePriority.rel_id;

    select max(priority) into priority2
      from scs_priority_rels
     where on_what_parent_table = on_what_parent_table1
       and on_which_parent_id = on_which_parent_id1
       and on_what_child_table = on_what_child_table1
       and priority < priority1;

    select rel_id into rel_id2
      from scs_priority_rels
     where priority = priority2
       and on_what_parent_table = on_what_parent_table1
       and on_which_parent_id = on_which_parent_id1
       and on_what_child_table = on_what_child_table1;

    swapPriorities(rel_id, rel_id2);

  exception
    when NO_DATA_FOUND then return ;
  end increasePriority;


  procedure decreasePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  )
  is
    priority1			integer;
    on_what_parent_table1	varchar2(100);
    on_which_parent_id1		integer;
    on_what_child_table1	varchar2(100);
    priority2			integer;
    rel_id2			integer;
  begin
    select on_what_parent_table, on_which_parent_id, 
	   on_what_child_table, priority 
      into on_what_parent_table1, on_which_parent_id1, 
	   on_what_child_table1, priority1
      from scs_priority_rels
     where rel_id = decreasePriority.rel_id;

    select min(priority) into priority2
      from scs_priority_rels
     where on_what_parent_table = on_what_parent_table1
       and on_which_parent_id = on_which_parent_id1
       and on_what_child_table = on_what_child_table1
       and priority > priority1;

    select rel_id into rel_id2
      from scs_priority_rels
     where priority = priority2
       and on_what_parent_table = on_what_parent_table1
       and on_which_parent_id = on_which_parent_id1
       and on_what_child_table = on_what_child_table1;

    swapPriorities(rel_id, rel_id2);

  exception
    when NO_DATA_FOUND then return ;
  end decreasePriority;

  function getTruePriority(
    rel_id	in scs_priority_rels.rel_id%TYPE
  ) return integer
  is
    priority			integer;
    on_what_parent_table	varchar2(100);
    on_which_parent_id		integer;
    on_what_child_table		varchar2(100);
    priority_count		integer;
  begin
    select on_what_parent_table,
	   on_which_parent_id,
	   on_what_child_table,
	   priority
      into getTruePriority.on_what_parent_table,
	   getTruePriority.on_which_parent_id,
	   getTruePriority.on_what_child_table,
	   getTruePriority.priority
      from scs_priority_rels
     where rel_id = getTruePriority.rel_id;

   select count(*) 
     into getTruePriority.priority_count
     from scs_priority_rels
    where on_what_parent_table = getTruePriority.on_what_parent_table
      and on_which_parent_id   = getTruePriority.on_which_parent_id
      and on_what_child_table  = getTruePriority.on_what_child_table
      and priority <= getTruePriority.priority;
  
    return getTruePriority.priority_count;

  exception
    when NO_DATA_FOUND then return null;
  end getTruePriority;

end scs_priority;
/
show errors


