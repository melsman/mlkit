/* ======================================================================
   test suite for scs_role package

   $Id$

   History: 
   291102 Kennie Nybo Pontoppidan <kennie@it-c.dk> 
   code review, added test cases and comments
   281002 Kennie Nybo Pontoppidan <kennie@it-c.dk> created test suite
====================================================================== */

set serveroutput on

declare
  illegal_id	integer;

  text_id1	integer;
  text_id2	integer;

  rand_name1	varchar2(10);
  rand_name2	varchar2(10);

  rid		integer;
  role_id1	integer;
  role_id2	integer;

  obj_id1	integer;

  party_id1	integer;
  party_id2	integer;

  clean_up	exception;

  counter1_b	integer;
  counter1_a	integer;
  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
  counter000_b	integer;
  counter000_a	integer;
  counter0000_b	integer;
  counter0000_a	integer;
begin
  scs_test.printl( '-------------------------' );
  scs_test.printl( 'testing scs_roles package' );
  scs_test.printl( '-------------------------' );

  select count(*) into counter0_b from scs_roles;
  select count(*) into counter00_b from scs_role_rels;
  select count(*) into counter000_b from scs_parties;
  select count(*) into counter0000_b from scs_texts;

  illegal_id := scs.new_obj_id;

  -- testing new
  scs_test.printl( 'testing function ''new'':' );
  -- legal values: no object_id provided
  select count (*) into counter1_b from scs_roles;
  text_id1 := scs_text.new;
  rand_name1 := scs_random.rand_string(10);
  role_id1 := scs_role.new( abbreviation => rand_name1, 
			    role_description_tid => text_id1 );
  select count (*) into counter1_a from scs_roles;
  scs_test.testBool( 'new', 1, counter1_a = counter1_b + 1 );  

  select role_id into rid from scs_roles
   where abbreviation = rand_name1
     and role_description_tid = text_id1;
  scs_test.testBool( 'new', 2, rid = role_id1 );  

  -- legal values: object_id provided
  obj_id1 := scs.new_obj_id;
  text_id2 := scs_text.new;
  rand_name2 := scs_random.rand_string(10);
  role_id2 := scs_role.new( obj_id1, rand_name2, text_id2 );
  select count (*) into counter1_b from scs_roles where abbreviation = rand_name2;
  scs_test.testBool( 'new', 3, role_id2 = obj_id1 and counter1_b = 1 );


  -- illegal values: illegal object_id
  scs_test.testExn( 'new', 4, '
    declare
      rid	integer;
    begin
      rid := scs_role.new( object_id		=> '|| role_id1 ||',
			   abbreviation	        => scs_random.rand_string(10),
			   role_description_tid => '|| text_id1 ||' );
    end;', 'f' );

  -- illegal values: illegal abbreviation
  scs_test.testExn( 'new', 5, '
    declare
      rid	integer;
    begin
      rid := scs_role.new( abbreviation	        => ''' || rand_name1 || ''',
			   role_description_tid => '|| text_id1 ||' );
    end;', 'f' );

    -- illegal values: null abbreviation
  scs_test.testExn( 'new', 6, '
    declare
      rid	integer;
    begin
      rid := scs_role.new( abbreviation	        => null,
			   role_description_tid => '|| text_id1 ||' );
    end;', 'f' );

  -- illegal values: role_description_tid was null
  scs_test.testExn( 'new', 7, '
    declare
      rid	integer;
    begin
      rid := scs_role.new( abbreviation	        => scs_random.rand_string(10),
			   role_description_tid => null );
    end;', 'f' );

  -- illegal values: role_description_tid was unknown
  scs_test.testExn( 'new', 8, '
    declare
      rid	integer;
    begin
      rid := scs_role.new( abbreviation	        => scs_random.rand_string(10),
			   role_description_tid => '|| illegal_id ||' );
    end;', 'f' );

  -- testing abbrev_to_roleid
  scs_test.printl( 'testing function ''abbrev_to_roleid'':' );
  -- known abbreviation
  scs_test.testBool('abbrev_to_roleid', 1, 
    scs_role.abbrev_to_roleid( rand_name1 ) = role_id1);
  -- unknown abbreviation
  scs_test.testExn('abbrev_to_roleid', 2, '
    declare
      roleid integer;
    begin
      roleid := scs_role.abbrev_to_roleid( ''DoesNotExist'' );
    end;', 'f' );

  -- testing add
  scs_test.printl( 'testing procedure ''add'':' );
  party_id1 := scs_party.new;
  party_id2 := scs_party.new;
  scs_role.add( party_id1, role_id1 );
  select count (*) into counter1_a from scs_role_rels 
   where party_id = party_id1 and role_id = role_id1;
  scs_test.testBool('add using roleid', 1, counter1_a = 1 );

  scs_role.add( party_id2, rand_name1 );
  select count (*) into counter1_a from scs_role_rels 
   where party_id = party_id2 
     and role_id = scs_role.abbrev_to_roleid( rand_name1);
  scs_test.testBool('add using abbreviation', 1, counter1_a = 1 );

  -- multiple adds
  scs_test.testUnit( 'add using roleid', 2, '
    declare
      i		integer;
    begin
      for i in 1..1000 loop
        scs_role.add( ' || party_id1 || ', ' || role_id1 || ' );
      end loop;
    end;' );  
  select count (*) into counter1_a from scs_role_rels 
   where party_id = party_id1 and role_id = role_id1;
  scs_test.testBool('add using roleid', 3, counter1_a = 1 );

  scs_test.testUnit( 'add using abbreviation', 2, '
    declare
      i		integer;
    begin
      for i in 1..1000 loop
        scs_role.add( ' || party_id2 || ', ''' || rand_name1 || ''' );
      end loop;
    end;' );  
  select count (*) into counter1_a from scs_role_rels 
   where party_id = party_id2
     and role_id = scs_role.abbrev_to_roleid( rand_name1);
  scs_test.testBool('add using abbreviation', 3, counter1_a = 1 );

  -- illegal values: unknown role_id
  scs_test.testExn('add using roleid', 4, '
    begin
      scs_role.add( party_id => ' || party_id1 || ', 
		    role_id => ' || illegal_id || ');
    end;','f');

  -- illegal values: unknown abbreviation
  scs_test.testExn('add using abbreviation', 4, '
    begin
      scs_role.add( party_id => ' || party_id1 || ', 
		    role_abbrev => scs_random.rand_string(10) );
    end;','f');

  -- illegal values: unknown party_id
  scs_test.testExn('add using roleid', 5, '
    begin
      scs_role.add( party_id => ' || illegal_id || ', role_id => ' || role_id1 || ');
    end;','f');

  -- illegal values: unknown party_id
  scs_test.testExn('add using abbreviation', 5, '
    begin
      scs_role.add( party_id => ' || illegal_id || ', 
		    role_abbrev => ''' || rand_name1 || ''' );
    end;','f');

  -- testing has_p
  scs_test.printl( 'testing function ''has_p'':' );
  -- first testing role_id version

  -- legal values: entry exists
  scs_test.testBool( 'has_p using roleid', 1, 
    scs_role.has_p( party_id => party_id1, role_id => role_id1 ) = 't');

  -- legal values: entry does not exist
  scs_test.testBool( 'has_p using roleid', 2, 
    scs_role.has_p( party_id => party_id1, role_id => role_id2 ) = 'f');

  -- illegal values: unknown party_id
  scs_test.testBool('has_p using roleid', 3, 
    scs_role.has_p( party_id => illegal_id, role_id => role_id1 ) = 'f');

  -- illegal values: null party_id
  scs_test.testBool('has_p using roleid', 4, 
    scs_role.has_p( party_id => null, role_id => role_id1 ) = 'f');

  -- illegal values: unknown role_id
  scs_test.testBool('has_p using roleid', 5, 
    scs_role.has_p( party_id => party_id1, role_id => illegal_id ) = 'f');

  -- illegal values: null role_id
  scs_test.testBool('has_p using roleid', 6, 
    scs_role.has_p( party_id => party_id1, role_id => null ) = 'f');

  -- illegal values: unknown party_id, unknown role_id
  scs_test.testBool('has_p using roleid', 7, 
    scs_role.has_p( party_id => illegal_id, role_id => illegal_id ) = 'f');

  -- illegal values: null party_id, null role_id
  scs_test.testBool('has_p using roleid', 7, 
    scs_role.has_p( party_id => null, role_id => null ) = 'f');

  -- now for abbreviation version
  -- legal values: entry exists
  scs_test.testBool( 'has_p using abbreviation', 1, 
    scs_role.has_p( party_id => party_id1, role_id => role_id1 ) = 't');

  -- legal values: entry does not exist
  scs_test.testBool( 'has_p using abbreviation', 2, 
    scs_role.has_p( party_id => party_id1, role_id => role_id2 ) = 'f');

  -- illegal values: unknown party_id
  scs_test.testBool('has_p using abbreviation', 3, 
    scs_role.has_p( party_id => illegal_id, role_id => role_id1 ) = 'f');

  -- illegal values: null party_id
  scs_test.testBool('has_p using abbreviation', 4, 
    scs_role.has_p( party_id => null, role_id => role_id1 ) = 'f');

  -- illegal values: unknown role_id
  scs_test.testBool('has_p using abbreviation', 5, 
    scs_role.has_p( party_id => party_id1, role_id => illegal_id ) = 'f');

  -- illegal values: null role_id
  scs_test.testBool('has_p using abbreviation', 6, 
    scs_role.has_p( party_id => party_id1, role_id => null ) = 'f');

  -- illegal values: unknown party_id, unknown role_id
  scs_test.testBool('has_p using abbreviation', 7, 
    scs_role.has_p( party_id => illegal_id, role_id => illegal_id ) = 'f');

  -- illegal values: null party_id, null role_id
  scs_test.testBool('has_p using abbreviation', 7, 
    scs_role.has_p( party_id => null, role_id => null ) = 'f');

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );
    delete scs_role_rels where party_id in ( party_id1, party_id2 );
    delete scs_parties where party_id in ( party_id1, party_id2 );
    delete scs_roles where role_id in ( role_id1, role_id2 );
    delete scs_texts where text_id in  ( text_id1, text_id2 );

    select count(*) into counter0_a from scs_roles;
    select count(*) into counter00_a from scs_role_rels;
    select count(*) into counter000_a from scs_parties;
    select count(*) into counter0000_a from scs_texts;
  
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end;
/
show errors


declare
  illegal_id	integer;

  text_id1	integer;
  text_id2	integer;
  text_id3	integer;
  text_id4	integer;
  text_id5	integer;
  text_id6	integer;
  text_id7	integer;

  rand_name1	varchar2(10);
  rand_name2	varchar2(10);
  rand_name3	varchar2(10);
  rand_name4	varchar2(10);
  rand_name5	varchar2(10);
  rand_name6	varchar2(10);
  rand_name7	varchar2(10);

  rid		integer;
  role_id1	integer;
  role_id2	integer;
  role_id3	integer;
  role_id4	integer;
  role_id5	integer;
  role_id6	integer;
  role_id7	integer;

  obj_id1	integer;

  party_id1	integer;
  party_id2	integer;

  clean_up	exception;

  counter1_b	integer;
  counter1_a	integer;
  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
  counter000_b	integer;
  counter000_a	integer;
  counter0000_b	integer;
  counter0000_a	integer;
begin
  scs_test.printl( '-------------------------' );
  scs_test.printl( 'testing scs_roles package' );
  scs_test.printl( '-------------------------' );

  select count(*) into counter0_b from scs_roles;
  select count(*) into counter00_b from scs_role_rels;
  select count(*) into counter000_b from scs_parties;
  select count(*) into counter0000_b from scs_texts;

  illegal_id := scs.new_obj_id;

  text_id1 := scs_text.new;
  rand_name1 := scs_random.rand_string(10);

  role_id1 := scs_role.new( abbreviation => rand_name1, 
			    role_description_tid => text_id1 );

  obj_id1 := scs.new_obj_id;
  text_id2 := scs_text.new;
  rand_name2 := scs_random.rand_string(10);
  role_id2 := scs_role.new( obj_id1, rand_name2, text_id2 );

  party_id1 := scs_party.new;
  party_id2 := scs_party.new;
  scs_role.add( party_id1, role_id1 );
  scs_role.add( party_id2, rand_name1 );

  -- testing remove
  -- first using role_id
  scs_test.printl( 'testing procedure ''remove'':' );

  -- legal values: party has role
  select count (*) into counter1_b 
    from scs_role_rels 
   where party_id = party_id1
     and role_id = role_id1;
  scs_role.remove( party_id => party_id1, role_id => role_id1 );
  select count (*) into counter1_a 
    from scs_role_rels 
   where party_id = party_id1
     and role_id = role_id1;
  scs_test.testBool( 'remove using role_id', 1, 
    counter1_b = 1 AND counter1_a = 0 );

  -- legal values: party doesn't have role
  select count (*) into counter1_b 
  from scs_role_rels where party_id = party_id1;
  scs_test.testUnit( 'remove using role_id', 2, '
    begin
      scs_role.remove(party_id => '||party_id1||', role_id => '||role_id1||');
    end;' );  
  select count (*) into counter1_a 
  from scs_role_rels where party_id = party_id1;
  scs_test.testBool( 'remove using role_id', 3, counter1_a = counter1_b );

  -- illegal values: unknown party_id
  scs_test.testUnit( 'remove using role_id', 4, '
    begin
      scs_role.remove( party_id => ' || illegal_id || ', 
		       role_id => ' || role_id1 || ');
    end;' );  

  -- illegal values: party_id is null
  scs_test.testUnit( 'remove using role_id', 5, '
    begin
      scs_role.remove( party_id => null, 
		       role_id => ' || role_id1 || ');
    end;' );  

  -- illegal values: unknown role_id
  scs_test.testUnit( 'remove using role_id', 6, '
    begin
      scs_role.remove( party_id => ' || party_id1 || ', 
		       role_id => ' || illegal_id || ');
    end;' );  

  -- illegal values: role_id is null 
  scs_test.testUnit( 'remove using role_id', 7, '
    begin
      scs_role.remove( party_id => ' || party_id1 || ', 
		       role_id => null );
    end;' );  

  -- illegal values: unknown party_id, unknown role_id
  scs_test.testUnit( 'remove using role_id', 8, '
    begin
      scs_role.remove( party_id => ' || illegal_id || ', 
		       role_id => ' || illegal_id || ');
    end;' );  

  -- illegal values: party_id is null, role_id is null 
  scs_test.testUnit( 'remove using role_id', 9, '
    begin
      scs_role.remove( party_id => null, 
		       role_id => null );
    end;' );  

  -- now using role_abbrev
  scs_role.add( party_id1, role_id1 );
  scs_role.add( party_id2, rand_name1 );
  -- legal values: party has role
  select count (*) into counter1_b 
    from scs_role_rels 
   where party_id = party_id1
     and role_id = scs_role.abbrev_to_roleid( rand_name1 );
  scs_role.remove( party_id => party_id1, role_abbrev => rand_name1 );
  select count (*) into counter1_a 
    from scs_role_rels 
   where party_id = party_id1
     and role_id = scs_role.abbrev_to_roleid( rand_name1 );
  scs_test.testBool( 'remove using abbreviation', 1, 
    counter1_b = 1 AND counter1_a = 0 );

  -- legal values: party doesn't have role
  select count (*) into counter1_b 
  from scs_role_rels where party_id = party_id1;
  scs_test.testUnit( 'remove using abbreviation', 2, '
    begin
      scs_role.remove( party_id => ' || party_id1 || ', 
		       role_abbrev => ''' || rand_name2 || ''');
    end;' );  
  select count (*) into counter1_a 
  from scs_role_rels where party_id = party_id1;
  scs_test.testBool( 'remove using abbreviation', 3, counter1_a = counter1_b );

  -- illegal values: unknown party_id
  scs_test.testUnit( 'remove using abbreviation', 4, '
    begin
      scs_role.remove( party_id => ' || illegal_id || ', 
		       role_abbrev => ' || role_id1 || ');
    end;' );  

  -- illegal values: party_id is null
  scs_test.testUnit( 'remove using abbreviation', 5, '
    begin
      scs_role.remove( party_id => null, 
		       role_abbrev => ''' || rand_name1 || ''');
    end;' );  

  -- illegal values: unknown abbreviation
  scs_test.testUnit( 'remove using abbreviation', 6, '
    begin
      scs_role.remove( party_id => ' || party_id1 || ', 
		       role_abbrev => scs_random.rand_string(10) );
    end;' );  

  -- illegal values: abbreviation is null 
  scs_test.testUnit( 'remove using abbreviation', 7, '
    begin
      scs_role.remove( party_id => ' || party_id1 || ', 
		       role_abbrev => null );
    end;' );  

  -- illegal values: unknown party_id, unknown abbreviation
  scs_test.testUnit( 'remove using abbreviation', 8, '
    begin
      scs_role.remove( party_id => ' || illegal_id || ', 
		       role_abbrev => scs_random.rand_string(10) );
    end;' );  

  -- illegal values: party_id is null, role_id is null 
  scs_test.testUnit( 'remove using abbreviation', 9, '
    begin
      scs_role.remove( party_id => null, 
		       role_abbrev => null );
    end;' );  

  -- testing destroy
  scs_test.printl( 'testing procedure ''destroy'':' );

  -- first using role_id
  rand_name4 := scs_random.rand_string(10);
  text_id4 := scs_text.new;
  role_id4 := scs_role.new ( abbreviation => rand_name4, 
			     role_description_tid => text_id4 );
  scs_role.add( party_id1, role_id4 ); -- make sure role4 is non-empty

  rand_name5 := scs_random.rand_string(10);
  text_id5 := scs_text.new;
  role_id5 := scs_role.new ( abbreviation => rand_name5, 
			     role_description_tid => text_id5 );

  -- legal value: non-empty role
  select count (*) into counter1_b from scs_roles;
  scs_test.testUnit( 'destroy, using role_id', 1, '
    begin
      scs_role.destroy ( role_id => ' || role_id4 || ' );     
    end;' );
  select count (*) into counter1_a from scs_roles;
  scs_test.testBool( 'destroy, using role_id', 2, counter1_a = counter1_b - 1 );

  -- legal value: destroying empty role
   scs_test.testUnit( 'destroy, using role_id', 3, '
    begin
      scs_role.destroy ( role_id => ' || role_id5 || ' );     
    end;' );

  -- illegal value: unknown role_id
   scs_test.testUnit( 'destroy, using role_id', 4, '
    begin
      scs_role.destroy ( role_id => ' || illegal_id || ' );     
    end;' );

  -- illegal value: role_id is null
   scs_test.testUnit( 'destroy, using role_id', 5, '
    begin
      scs_role.destroy ( role_id => null );     
    end;' );  

  -- now using abbreviation
  text_id6 := scs_text.new;
  rand_name6 := scs_random.rand_string(10);
  role_id6 := scs_role.new ( abbreviation => rand_name6, 
			     role_description_tid => text_id6 );
  scs_role.add( party_id1, role_id6 ); -- make sure role1 is non-empty

  rand_name7 := scs_random.rand_string(10);
  text_id7 := scs_text.new;
  -- role7 is empty
  role_id7 := scs_role.new ( abbreviation => rand_name7, 
			     role_description_tid => text_id7 );

  -- legal value: non-empty role
  select count (*) into counter1_b from scs_roles;
  scs_test.testUnit( 'destroy, using abbreviation', 6, '
    begin
      scs_role.destroy ( abbreviation => ''' || rand_name1 || ''' );     
    end;' );
  select count (*) into counter1_a from scs_roles;
  scs_test.testBool( 'destroy, using abbreviation', 7, counter1_a = counter1_b - 1 );

  -- legal value: destroying empty role
   scs_test.testUnit( 'destroy, using abbreviation', 8, '
    begin
      scs_role.destroy ( abbreviation => ''' || rand_name3 || ''' );     
    end;' );

  -- illegal value: unknown abbreviation
   scs_test.testUnit( 'destroy, using abbreviation', 9, '
    begin
      scs_role.destroy ( abbreviation => scs_random.rand_string(10) );     
    end;' );

  -- illegal value: abbreviation is null
   scs_test.testUnit( 'destroy, using abbreviation', 10, '
    begin
      scs_role.destroy ( abbreviation => null );     
    end;' );  

  -- illegal value: role_id is null, abbreviation is null
   scs_test.testUnit( 'destroy', 11, '
    begin
      scs_role.destroy ( role_id => null, abbreviation => null );     
    end;' );  

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );
    delete scs_role_rels where party_id in ( party_id1, party_id2 );
    delete scs_parties where party_id in ( party_id1, party_id2 );
    delete scs_roles 
     where role_id in ( 
       role_id1, role_id2, role_id3, role_id4, role_id5,
       role_id6, role_id7 );
    delete scs_texts 
     where text_id in ( 
       text_id1, text_id2, text_id3, text_id4, text_id5,
       text_id6, text_id7 );

    select count(*) into counter0_a from scs_roles;
    select count(*) into counter00_a from scs_role_rels;
    select count(*) into counter000_a from scs_parties;
    select count(*) into counter0000_a from scs_texts;
  
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end;
/
show errors
