/*
 test of scs_role package

 $Id$

*/

set serveroutput on
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
  scs_test.printl('[Testing Roles...]');
  -- testing new
  textid1 := scs_text.new;
  roleid1 := scs_role.new( abbreviation => 'BOFH', role_description_tid => textid1 );
  select count (*) into counter1 from scs_roles where abbreviation = 'BOFH';
  scs_test.testBool( 'new', 1, counter1 = 1 );  

  objid1 := scs.new_obj_id;
  roleid2 := scs_role.new( objid1, 'dba', textid1 );
  select count (*) into counter1 from scs_roles where abbreviation = 'dba';
  scs_test.testBool( 'new', 2, roleid2 = objid1 and counter1 = 1 );

  -- testing abbrev_to_roleid
  scs_test.testBool('abbrev_to_roleid',1,scs_role.abbrev_to_roleid('BOFH') = roleid1);
  scs_test.testBool('abbrev_to_roleid',2,scs_role.abbrev_to_roleid('dba') = roleid2);
  scs_test.testExn('abbrev_to_roleid',3,'
                   declare
                     roleid integer;
                   begin
                     roleid := scs_role.abbrev_to_roleid(''DoesNotExist'');
                   end;','f');

  -- testing add
  partyid0 := 0;
  partyid1 := 1;
  scs_role.add( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels where party_id = partyid0 and role_id = roleid1;
  scs_test.testBool('add using roleid', 1, counter1 = 1 );

  scs_role.add( partyid0, roleid1 );
  select count (*) into counter1 from scs_role_rels where party_id = partyid0 and role_id = roleid1;
  scs_test.testBool('add using roleid', 2, counter1 = 1 );

  scs_test.testExn('add using roleid',3,'
                   begin
                     scs_role.add(party_id => ' || partyid0 || ', role_id => ''42424242'');
                   end;','f');

  scs_test.testExn('add using roleid',4,'
                   begin
                     scs_role.add(party_id => ''42424242'', role_id => ' || roleid2 || ');
                   end;','f');

  -- testing has_p
  scs_test.testBool('has_p using roleid', 1, scs_role.has_p(party_id => partyid0, role_id => roleid1) = 't');
  scs_test.testBool('has_p using roleid', 2, scs_role.has_p(party_id => partyid0, role_id => roleid2) = 'f');
  scs_test.testBool('has_p using roleid', 3, scs_role.has_p(party_id => partyid0, role_id => '42424242') = 'f');
  scs_test.testBool('has_p using roleid', 4, scs_role.has_p(party_id => 42424242, role_id => roleid1) = 'f'); 
	
  -- testing remove using role_id
  select count (*) into counter1 from scs_role_rels where party_id = partyid1;
  scs_test.testBool('remove using role_id',1, counter1 = 1);

  scs_role.remove(party_id => partyid0, role_id => roleid1);
  select count (*) into counter1 from scs_role_rels where party_id = partyid0 and role_id = roleid1;
  scs_test.testBool('remove using role_id',2, counter1 = 0);

  scs_role.remove(party_id => partyid0, role_id => roleid1);
  select count (*) into counter1 from scs_role_rels where party_id = partyid0 and role_id = roleid1;
  scs_test.testBool('remove using role_id',3, counter1 = 0);

  scs_role.remove(party_id => 42424242, role_id => roleid1);  -- party does not exists
  select count (*) into counter1 from scs_role_rels where party_id = 42424242 and role_id = roleid1;
  scs_test.testBool('remove using role_id',4, counter1 = 0);

  scs_role.remove(party_id => partyid0, role_id => 42424242);  -- role_id does not exists
  select count (*) into counter1 from scs_role_rels where party_id = partyid0 and role_id = 42424242;
  scs_test.testBool('remove using role_id',5, counter1 = 0);

  -- testing add again using abbreviations instead of roleid's
  scs_role.add(partyid0, 'BOFH');
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('add using abbrev', 1, counter1 = 1 );

  scs_role.add(partyid0, 'BOFH');
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('add using abbrev', 2, counter1 = 1 );

  scs_test.testExn('add using abbrev',3,'
                   begin
                     scs_role.add(party_id => ' || partyid0 || ', role_abbrev => ''unknown'');
                   end;','f');

  scs_test.testExn('add using abbrev',4,'
                   begin
                     scs_role.add(party_id => ''42424242'', role_abbrev => ''BOFH'');
                   end;','f');

  -- testing has_p using abbrev
  scs_test.testBool( 'has_p using abbrev', 1, scs_role.has_p( party_id => partyid0, role_abbrev => 'BOFH' ) = 't' );
  scs_test.testBool( 'has_p using abbrev', 2, scs_role.has_p( party_id => partyid0, role_abbrev => 'dbs' ) = 'f' );
  scs_test.testBool( 'has_p using abbrev', 3, scs_role.has_p( party_id => partyid0, role_abbrev => 'unknown' ) = 'f' );
  scs_test.testBool( 'has_p using abbrev', 4, scs_role.has_p( party_id => 42424242, role_abbrev => 'BOFH' ) = 'f' ); 

  -- testing remove using role_abbrev
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('remove using role_abbrev',1, counter1 = 1);

  scs_role.remove(party_id => partyid0, role_abbrev => 'BOFH');
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('remove using role_abbrev',2, counter1 = 0);

  scs_role.remove(party_id => partyid0, role_abbrev => 'BOFH');
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('remove using role_abbrev',3, counter1 = 0);

  scs_role.remove(party_id => 42424242, role_abbrev => 'BOFH');  -- party does not exists
  select count (*) into counter1 from scs_role_rels where party_id = 42424242;
  scs_test.testBool('remove using role_abbrev',4, counter1 = 0);

  scs_role.remove(party_id => partyid0, role_abbrev => '42424242');  -- role_abbrev does not exists
  select count (*) into counter1 from scs_role_rels where party_id = partyid0;
  scs_test.testBool('remove using role_abbrev',5, counter1 = 0);

  -- testing destroy
  select count (*) into counter2 from scs_roles;
  scs_role.destroy ( abbreviation => 'BOFH' ); --he he

  scs_role.destroy ( role_id => roleid2 ); 
  select count (*) into counter1 from scs_roles;
  scs_test.testBool( 'destroy', 1, counter2-2 = counter1 );  


end;
/
show errors
