/* ======================================================================
   test suite for scs_priority package

   $Id$

   History: 
   2003-12-29 Kennie Nybo Pontoppidan created test suite
====================================================================== */

set serveroutput on

declare
  counter0_b integer;
  counter0_a integer;
  counter1_b integer;
  counter1_a integer;

  rel_id1    integer;
  rel_id2    integer;

  priority1_b    integer;
  priority2_b    integer;
  priority1_a    integer;
  priority2_a    integer;

  rand_name1 varchar2(30);
  rand_name2 varchar2(30);
  rand_name3 varchar2(30);
  rand_name4 varchar2(30);

  rand1 number;
  rand2 number;
  rand3 number;
  rand4 number;

  clean_up		exception;
begin
  scs_test.printl( '----------------------------' );
  scs_test.printl( 'testing package scs_priority' );
  scs_test.printl( '----------------------------' );

  select count(*) into counter0_b from scs_priority_rels;

  rand_name1 := scs_random.rand_string(30);
  rand_name2 := scs_random.rand_string(30);
  rand_name3 := scs_random.rand_string(30);
  rand_name4 := scs_random.rand_string(30);

  rand1 := scs_random.rand_max(10000000000);
  rand2 := scs_random.rand_max(10000000000);
  rand3 := scs_random.rand_max(10000000000);
  rand4 := scs_random.rand_max(10000000000);

  scs_test.printl( 'testing function ''swapPriorities'':' );

  -- illegal values first:

  -- non-matching ON_WHAT_PARENT_TABLE
  rel_id1 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id1,
    rand_name1,
    rand1,
    rand_name2,
    rand2,
    0    
  );

  rel_id2 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id2,
    rand_name2,
    rand1,
    rand_name2,
    rand2,
    0    
  );

  scs_test.testExn( 'swapPriorities', 1, '
    begin
      scs_priority.swapPriorities( ' || rel_id1 || ',' || rel_id2 || ' ); 
    end; ', 'f' );

  -- matching ON_WHAT_PARENT_TABLE,
  -- non-matching ON_WHICH_PARENT_ID
  rel_id1 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id1,
    rand_name1,
    rand1,
    rand_name1,
    rand2,
    0    
  );

  rel_id2 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id2,
    rand_name1,
    rand2,
    rand_name2,
    rand2,
    0    
  );

  scs_test.testExn( 'swapPriorities', 2, '
    begin
      scs_priority.swapPriorities( ' || rel_id1 || ',' || rel_id2 || ' ); 
    end; ', 'f' );

  -- testing unknown rel_id1
  rel_id1 := scs.new_obj_id;
  scs_test.testExn( 'swapPriorities', 3, '
    begin
      scs_priority.swapPriorities( ' || rel_id1 || ',' || rel_id2 || ' ); 
    end; ', 'f' );


  -- matching ON_WHAT_PARENT_TABLE, ON_WHICH_PARENT_ID
  -- non-matching ON_WHAT_CHILD_TABLE
  rel_id1 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id1,
    rand_name1,
    rand1,
    rand_name3,
    rand2,
    0    
  );

  rel_id2 := scs.new_obj_id;
  insert into scs_priority_rels(
    REL_ID 		,
    ON_WHAT_PARENT_TABLE,
    ON_WHICH_PARENT_ID	,
    ON_WHAT_CHILD_TABLE	,
    ON_WHICH_CHILD_ID	,
    MODIFYING_USER 	
  ) values (
    rel_id2,
    rand_name1,
    rand1,
    rand_name4,
    rand2,
    0    
  );

  scs_test.testExn( 'swapPriorities', 4, '
    begin
      scs_priority.swapPriorities( ' || rel_id1 || ',' || rel_id2 || ' ); 
    end; ', 'f' );

  -- testing unknown rel_id2
  rel_id2 := scs.new_obj_id;
  scs_test.testExn( 'swapPriorities', 5, '
    begin
      scs_priority.swapPriorities( ' || rel_id1 || ',' || rel_id2 || ' ); 
    end; ', 'f' );

  -- legal values : 
  rel_id1 := scs_priority.new(
    rand_name3,
    rand3,
    rand_name4,
    rand3,
    0
  );

  rel_id2 := scs_priority.new(
    rand_name3,
    rand3,
    rand_name4,
    rand3,
    0
  );

  select priority into priority1_b
    from scs_priority_rels
   where rel_id = rel_id1;

  select priority into priority2_b
    from scs_priority_rels
   where rel_id = rel_id2;

  scs_priority.swapPriorities( rel_id1, rel_id2);

  select priority into priority1_a
    from scs_priority_rels
   where rel_id = rel_id1;

  select priority into priority2_a
    from scs_priority_rels
   where rel_id = rel_id2;

  scs_test.testBool( 'swapPriorities', 6, 
    priority1_a = priority2_b and 
    priority2_a = priority1_b );

  raise clean_up;
exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );

    delete scs_priority_rels where ON_WHAT_PARENT_TABLE in(
      rand_name1, rand_name2, rand_name3
    );

    select count(*) into counter0_a from scs_priority_rels;
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );

  when others then
  scs_test.print( 'an error occured: ' );
  scs_test.printl( SQLERRM );
  goto clean;

end;
/
show errors
