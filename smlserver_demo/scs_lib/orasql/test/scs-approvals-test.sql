/* ======================================================================
   test suite for scs_approval package

   $Id$

   History: 
   160303 Kennie Nybo Pontoppidan created test suite
====================================================================== */

set serveroutput on

declare
  counter0_b integer;
  counter0_a integer;
  counter1_b integer;
  counter1_a integer;

  rand_name varchar2(30);

  clean_up		exception;
begin
  scs_test.printl( '------------------------' );
  scs_test.printl( 'testing package scs_text' );
  scs_test.printl( '------------------------' );

  select count(*) into counter0_b from scs_approvals;

  rand_name := scs_random.rand_string(30);

  scs_test.printl( 'testing function ''approve_row'':' );
  select count(*) into counter1_b from scs_approvals;
  scs_approval.approve_row( table_name => rand_name, id => 42, approved_by => 0);
  select count(*) into counter1_a from scs_approvals;
  scs_test.testBool( 'approve_row', 1, counter1_a = counter1_b + 1 );
  select count(*) into counter1_a 
    from scs_approvals
   where ON_WHAT_TABLE = rand_name
     and ON_WHAT_ID = 42
     and PARTY_ID = 0
     and DECISION = 't';
  scs_test.testBool( 'approve_row', 2, counter1_a = 1 );

  scs_test.printl( 'testing function ''decline_row'':' );
  select count(*) into counter1_b from scs_approvals;
  scs_approval.decline_row( table_name => rand_name, id => 42, approved_by => 0, note_text => 'Neeej');
  select count(*) into counter1_a from scs_approvals;
  scs_test.testBool( 'decline_row', 1, counter1_a = counter1_b + 1 );
  select count(*) into counter1_a 
    from scs_approvals
   where ON_WHAT_TABLE = rand_name
     and ON_WHAT_ID = 42
     and PARTY_ID = 0
     and DECISION = 'f'
     and NOTE_TEXT = 'Neeej';
  scs_test.testBool( 'decline_row', 2, counter1_a = 1 );

  scs_test.printl( 'testing function ''delete_rows'':' );
    select count(*) into counter1_b from scs_approvals;
  scs_approval.delete_rows( table_name => rand_name, id => 42 );
  select count(*) into counter1_a from scs_approvals;
  scs_test.testBool( 'delete_rows', 1, counter1_a = counter1_b - 2 );
  select count(*) into counter1_a
    from scs_approvals
   where ON_WHAT_TABLE = rand_name
     and ON_WHAT_ID = 42;
  scs_test.testBool( 'delete_rows', 2, counter1_a = 0 );


  raise clean_up;
exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );

    delete scs_approvals where ON_WHAT_TABLE = rand_name;

    select count(*) into counter0_a from scs_approvals;
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );

  when others then
  scs_test.print( 'an error occured: ' );
  scs_test.printl( SQLERRM );
  goto clean;

end;
/
show errors