/* ======================================================================
   test suite for scs_parties package

   $Id$

   History: 
   151102 Kennie Nybo Pontoppidan created test suite
====================================================================== */


declare
  pid1 integer;
  pid2 integer;
  pid3 integer;
  pid4 integer;
  pid5 integer;

  counter integer;
  counter_b integer;
  counter1_b integer;
  counter_a integer;
  counter1_a integer;

begin
  scs_test.printl( '---------------------------' );
  scs_test.printl( 'testing scs_parties package' );
  scs_test.printl( '---------------------------' );
  select count(*) into counter_b from scs_parties;

  scs_test.printl( 'testing procedure new:' );
  select count(*) into counter1_b from scs_parties;
  pid1 := scs.new_obj_id;
  pid1 := scs_party.new( party_id => pid1, email=> 'test' || pid1 ||'@it-c.dk' );
  select count(*) into counter1_a from scs_parties;
  scs_test.testBool( 'new', 1, counter1_a = counter1_b +1 );
  scs_test.testExn( 'new', 2, '
  declare 
    pid integer;
  begin
    pid := scs_party.new( email=> ''test' || pid1 ||'@it-c.dk'' );
  end;', 'f' );

  scs_test.printl( 'testing procedure destroy:' );
  scs_party.destroy( party_id => pid1 );
  select count(*) into counter
  from scs_parties
  where deleted_p = 't'
  and email = to_char(pid1) || '-' || 'test'|| to_char(pid1) ||'@it-c.dk';
  -- test that deleted_p is set and email has the right form
  scs_test.testBool( 'destroy', 1, counter = 1 );
  pid2 := scs_party.new;
  -- test multiple destroys
  scs_test.testUnit( 'destroy', 2, '
    declare
      i		integer;
    begin
      for i in 1..1000 loop
        scs_party.destroy( party_id => ' || to_char(pid2) || ' );
      end loop;
    end;' );
  -- illegal party_id
  scs_test.testUnit( 'destroy', 3, '
    declare
      pid	integer;
    begin
      pid := scs.new_obj_id;
      scs_party.destroy( party_id => pid );
    end;' );
  -- long email is cut of
  pid3 := scs_party.new( email => 'kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk@it-c.dk' );  
  scs_test.testUnit( 'destroy', 4, '
    begin
      scs_party.destroy( party_id => ' || to_char(pid3) || ' );
    end;' );

  scs_test.printl( 'testing function email:' );
  pid4 := scs_party.new( email => 'test-email' );
  -- legal party_id
  scs_test.testBool( 'email', 1, scs_party.email(pid4) = 'test-email' );
  -- illegal party_id
  scs_test.testExn( 'email', 2, '
    declare
      str varchar(150);
    begin
      str := scs_party.email( scs.new_obj_id );
    end;', 'f' );

  scs_test.printl( 'testing function url:' );
  pid5 := scs_party.new( url => 'test-url' );
  -- legal party_id
  scs_test.testBool( 'url', 1, scs_party.url(pid5) = 'test-url' );
  -- illegal party_id
  scs_test.testExn( 'url', 2, '
    declare
      str varchar(150);
    begin
      str := scs_party.url( scs.new_obj_id );
    end;', 'f' );


  -- cleaning up
  scs_test.printl( 'cleaning up:' );
  delete from scs_parties where party_id in ( pid1, pid2, pid3, pid4, pid5 );
  select count(*) into counter_a from scs_parties;
  scs_test.testBool( 'garbage count', 1, counter_b = counter_a );
end;
/
show errors

