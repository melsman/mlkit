/*
 test of scs_role package

 $Id$

*/
declare
  textid1 integer;

  roleid1 integer;
  roleid2 integer;

  objid1 integer;

  counter1 integer;
  counter2 integer;

  partyid0 integer;
  partyid1 integer;
begin
  scs_test.printl('Testing Roles...');
  -- testing new
  textid1 := scs_text.new;
  roleid1 := scs_role.new( abbreviation => 'BOFH', role_description_tid => textid1 );
  select count (*) into counter1 from scs_roles;
  scs_test.testBool( 'new', 1, counter1 = 1 );  

  objid1 := scs.new_obj_id;
  roleid2 := scs_role.new( objid1, 'dba', textid1 );
  select count (*) into counter1 from scs_roles;
  scs_test.testBool( 'new', 2, roleid2 = objid1 and counter1 = 2 );

  -- testing add
  partyid0 := 0;
  partyid1 := 1;
  scs_role.add( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels;
  scs_test.testBool( 'add', 1, counter1 = 1 );

  scs_role.add( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels;
  scs_test.testBool( 'add', 2, counter1 = 1 );

  -- testing has_p
  scs_test.testBool( 'has_p', 1, scs_role.has_p( partyid0, roleid1 ) = 't' );
  scs_test.testBool( 'has_p', 2, scs_role.has_p( partyid0, roleid2 ) = 'f' );
	
  -- testing remove
  scs_role.remove( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels;
  scs_test.testBool( 'remove',1, counter1 = 0);

  scs_role.remove( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels;
  scs_test.testBool( 'remove',2, counter1 = 0);

  -- testing destroy
  select count (*) into counter2 from scs_roles;
  scs_role.destroy ( abbreviation => 'BOFH' ); --he he
  scs_role.destroy ( role_id => roleid2 ); 
  select count (*) into counter1 from scs_roles;
  scs_test.testBool( 'destroy', 1, counter2-2 = counter1 );  
end;
/
show errors
