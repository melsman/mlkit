-- $id$

set serveroutput on

declare
  pid1		integer;
  n		integer;

  counter_b	integer;
  counter1_b	integer;
  counter1_a	integer;
  counter_a	integer;
begin
  scs_test.printl( '-------------------------------' );
  scs_test.printl( 'testing scs_person package' );
  scs_test.printl( '-------------------------------' );
  select count(*) into counter_b from scs_persons;

  scs_test.printl( 'testing function new:' );

  pid1 := scs_person.new( first_names	=> 'Niels', 
                         last_name	=> 'Hallenberg', 
			 email		=> 'nh@it-c.dk',
                         modifying_user => scs_user.system);
  -- create dublicate Person
  scs_test.testExn( 'new', 1, '
    declare
      p integer;
    begin
      p := scs_person.new( person_id	  => ' || p || ',
                           first_names	  => ''Niels'', 
                           last_name	  => ''Hallenberg'', 
                           modifying_user => scs_user.system );
    end;', 'f' );
  -- create person with email already in use
  scs_test.testExn( 'new', 1, '
    declare
      p integer;
    begin
      p := scs_person.new( email	  => 'nh@it-c.dk',
                           first_names	  => ''Niels'', 
                           last_name	  => ''Hallenberg'', 
                           modifying_user => scs_user.system );
    end;', 'f' );

  /*

  scs_test.testBool('First Names', 1, scs_person.first_names(p) = 'Niels');
  scs_test.testBool('Last Name', 1, scs_person.last_name(p) = 'Hallenberg');
  scs_test.testBool('Normalise Name', 1, scs_person.norm_name('Niels','Hallenberg') = 'nielshallenberg');
  scs_test.testBool('Normalise Name', 2, scs_person.norm_name('','Hallenberg') = 'hallenberg');
  scs_test.testBool('Normalise Name', 3, scs_person.norm_name(' ','Hallenberg') = 'hallenberg');
  scs_test.testBool('Normalise Name', 4, scs_person.norm_name(' niels ','Hallenberg') = 'nielshallenberg');
  scs_test.testBool('Normalise Name', 5, scs_person.norm_name(' niels igen','Hallenberg') = 'nielshallenberg');
  scs_test.testBool('Normalise Name', 6, scs_person.norm_name('niels','Hallenberg hal') = 'nielshallenberghal');
  scs_test.testBool('Normalise Name', 7, scs_person.norm_name('niels','') = 'niels');
  scs_test.testBool('Normalise Name', 8, scs_person.norm_name(' ','') is null);
  scs_test.testBool('Normalise Name', 9, 
    scs_person.norm_name(' niels henrik pedersen','Hallenberg') = 'nielshallenberg');
  scs_test.testBool('Normalise Name', 10, 
    scs_person.norm_name(' nie ls     henri    k pedersen   ','Hallenberg') = 'niehallenberg');
  scs_test.testBool('Normalise Name', 11, scs_person.norm_name(p) = 'nielshallenberg');

  scs_test.printl('[Testing scs_person_rel package...]');
  scs_test.testUnit('Add',1,'begin scs_person_rel.add(' || p || ',''MyTable'',1); end;');
  scs_test.testUnit('Add',2,'begin scs_person_rel.add(' || p || ',''MyTable2'',1); end;');
  /* Duplicate index */
  scs_test.testUnit('Add',3,'begin scs_person_rel.add(' || p || ',''MyTable'',1); end;');
  scs_test.testUnit('Add',4,'begin scs_person_rel.add(' || p || ',''MyTable2'',2); end;');
  /* Non existing person - should fail*/
  scs_test.testExn('Add',5,'begin scs_person_rel.add(42,''MyTable3'',2); end;','f'); 
  select count(*) into n from scs_person_rels;
  scs_test.testBool('Add',6,n = 3);

  scs_test.printl('[Testing scs_person_rel package...]');
  scs_test.testUnit('Delete',1,'begin scs_person_rel.delete(' || p || ',''MyTable'',1); end;');
  scs_test.testUnit('Delete',2,'begin scs_person_rel.delete(' || p || ',''MyTable2'',1); end;');
  /* The next is already deleted */
  scs_test.testUnit('Delete',3,'begin scs_person_rel.delete(' || p || ',''MyTable'',1); end;');   
  scs_test.testUnit('Delete',4,'begin scs_person_rel.delete(' || p || ',''MyTable2'',2); end;');
  /* Non existing person - should not fail*/
  scs_test.testUnit('Delete',5,'begin scs_person_rel.delete(42,''MyTable2'',2); end;'); 
  select count(*) into n from scs_person_rels;
  scs_test.testBool('Delete',6,n = 0);

  scs_test.testUnit('Add',7,'begin scs_person_rel.add(' || p || ',''MyTable'',1); end;');
  scs_test.testUnit('Add',8,'begin scs_person_rel.add(' || p || ',''MyTable2'',1); end;');
  scs_test.testUnit('Delete All',1,'begin scs_person_rel.delete_all(' || p || '); end;');
  scs_test.testUnit('Delete All',2,'begin scs_person_rel.delete_all(' || p || '); end;');
  /* Non existing person - should not fail*/
  scs_test.testUnit('Delete All',3,'begin scs_person_rel.delete_all(42); end;'); 
  select count(*) into n from scs_person_rels;
  scs_test.testBool('Delete All',4,n = 0);

  scs_test.printl('[Testing destroy and delete...]');  
  scs_test.testBool('Deleted_p',1,scs_person.deleted_p(p) = 'f');
  scs_test.testUnit('Destroy', 1, 'begin scs_person.destroy(' || p || '); end;');
  scs_test.testBool('Deleted_p',2,scs_person.deleted_p(p) = 't');
  scs_test.testUnit('Destroy', 2, 'begin scs_person.destroy(' || p || '); end;');
  /* A non-existing person */
  scs_test.testUnit('Destroy', 3, 'begin scs_person.destroy(43); end;'); 

  */

  -- cleaning up
  scs_test.printl( 'cleaning up...' );
  delete scs_person where person_id in ( pid1 );
  select count(*) into counter_a from scs_persons;
  scs_test.testBool( 'garbage check', 1, counter_b = counter_a );
end;
/ 
show errors
