-- This code is a modified version of the acs-groups-test module found
-- in openACS (www.openacs.org): files acs-groups-test.sql

column log_key format a20;
column message format a90;
column ancestor_rel_type format a15;
column container_grp format a15;
column grp format a15;
column party format a15;
set lines 100

set serveroutput on

create or replace procedure print_scs_grp_party_index 
is
begin
  scs_log.notice('GT','| Group |      Party | Rel_id | Container | Ancestor |');
  for row in (select scs_group.name(grp_id) as grp, 
                     scs_party.email(party_id) as party, 
                     rel_id, 
                     scs_group.name(container_id) as container_grp,
                     ancestor_rel_type
                from scs_grp_party_index
               order by rel_id) loop
    scs_log.notice('GT','| ' || lpad(row.grp,5) || ' | ' || lpad(row.party,10) || ' | ' || 
                   lpad(row.rel_id,6) || ' | ' || 
                   lpad(row.container_grp,9) || ' | ' || lpad(row.ancestor_rel_type,8) );
  end loop;
end print_scs_grp_party_index;
/
show errors

declare
  A      integer;
  B      integer;
  C      integer;
  D      integer;
  E      integer;
  F      integer;
  G      integer;

  joe    integer;
  jane   integer;
  bob    integer;
  betty  integer;
  jack	 integer;
  jill	 integer;
  sven	 integer;
  stacy	 integer;

  rel_id        integer;
  B_rel_id_C    integer;
  A_rel_id_C    integer;
  D_rel_id_jane integer;
  B_rel_id_C_member integer;

  n_rows integer;
begin
  scs_test.print('[Create the test groups...');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  C := scs_group.new(grp_name => 'C', email => 'C', modifying_user => scs_user.system);
  D := scs_group.new(grp_name => 'D', email => 'D', modifying_user => scs_user.system);
  E := scs_group.new(grp_name => 'E', email => 'E', modifying_user => scs_user.system);
  F := scs_group.new(grp_name => 'F', email => 'F', modifying_user => scs_user.system);
  G := scs_group.new(grp_name => 'G', email => 'G', modifying_user => scs_user.system);
  scs_test.printl(']');

  -- Create the test members.
  scs_test.print('[Create users...');
  joe   := scs_user.new(email => 'joe@asdf.com',
	                first_names => 'p1 Joe', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jane  := scs_user.new(email => 'jane@asdf.com',
	                first_names => 'p2 Jane', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  bob   := scs_user.new(email => 'bob@asdf.com',
	                first_names => 'p3 Bob', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  betty := scs_user.new(email => 'betty@asdf.com',
	                first_names => 'p4 Betty', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jack  := scs_user.new(email => 'jack@asdf.com',
	                first_names => 'Jack', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jill  := scs_user.new(email => 'jill@asdf.com',
	                first_names => 'Jill', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  sven  := scs_user.new(email => 'sven@asdf.com',
	                first_names => 'Sven', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  stacy := scs_user.new(email => 'stacy@asdf.com',
	                first_names => 'Stacy', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  scs_test.printl(']');

  delete from scs_logs;

  scs_test.print('[Build member and composition relations (se test case in the documentation)...');
  rel_id := scs_grp_composition_rel.new(grp_id_one => A, grp_id_two => B, modifying_user => scs_user.system);
  B_rel_id_C := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => C, modifying_user => scs_user.system);
  rel_id := scs_grp_member_rel.new(grp_id => C, party_id => bob, modifying_user => scs_user.system);
  D_rel_id_jane := scs_grp_member_rel.new(grp_id => D, party_id => jane, modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => D, modifying_user => scs_user.system);
  rel_id := scs_grp_member_rel.new(grp_id => C, party_id => jane, modifying_user => scs_user.system);
  rel_id := scs_grp_member_rel.new(grp_id => D, party_id => joe, modifying_user => scs_user.system);
  A_rel_id_C := scs_grp_composition_rel.new(grp_id_one => A, grp_id_two => C, modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => C, grp_id_two => E, modifying_user => scs_user.system);
  rel_id := scs_grp_member_rel.new(grp_id => E, party_id => betty, modifying_user => scs_user.system);
  scs_test.printl(']');

  -- Test size of scs_grp_party_index - we expects 25 rows.
  select count(*) 
    into n_rows
    from scs_grp_party_index;
  scs_test.testBool('Size of grp_party_index', 1, n_rows = 25);

  --  DC = Direct Component of
  --   C = Component of'
  --  DM = Direct Member of
  --   M = Member of
  scs_test.testBool('Memberships', 1, 
  /* joe DM D(t) */ scs_group.member_p(party_id => joe, grp_id => D, cascade_membership=>'f') = 't' AND
  /* joe DM B(f) */ scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* joe  M B(t) */ scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'t') = 't' AND
  /* joe DM A(f) */ scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* joe  M A(t) */ scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'t') = 't');

  scs_test.testBool('Memberships', 2, 
  /* jane  M C(t) */  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  /* jane DM C(t) */  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  /* jane  M D(t) */  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 't' AND
  /* jane DM D(t) */  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 't' AND
  /* jane  M B(t) */  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 't' AND
  /* jane DM B(f) */  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* jane  M A(t) */  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 't' AND
  /* jane DM A(f) */  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  scs_test.testBool('Memberships', 3, 
  /* bob DM C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  /* bob  M C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  /* bob DM A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* bob  M A(t) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 't' AND
  /* bob DM B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* bob  M B(t) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 't');

  scs_test.testBool('Memberships', 4, 
  /* betty DM E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f') = 't' AND
  /* betty  M E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t') = 't' AND
  /* betty DM C(f) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f') = 'f' AND
  /* betty  M C(t) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t') = 't' AND
  /* betty DM B(f) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* betty  M B(t) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t') = 't' AND
  /* betty DM A(f) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* betty  M A(t) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t') = 't');

  scs_test.printl('[Remove B<--C composition and check membership for bob and betty]');  
  scs_grp_composition_rel.delete(B_rel_id_C, scs_user.system);
  scs_test.testBool('Check Representation', 1, scs_group.check_representation_all = 't');

  scs_test.testBool('Memberships', 5, 
  /* bob DM C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  /* bob  M C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  /* bob DM A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* bob  M A(t) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 't' AND
  /* bob DM B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* bob  M B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 'f');

  scs_test.testBool('Memberships', 6, 
  /* betty DM E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f') = 't' AND
  /* betty  M E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t') = 't' AND
  /* betty DM C(f) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f') = 'f' AND
  /* betty  M C(t) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t') = 't' AND
  /* betty DM B(f) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* betty  M B(f) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t') = 'f' AND
  /* betty DM A(f) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* betty  M A(t) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t') = 't');

  scs_test.printl('[Remove A<--C composition and check membership for bob and jane]');
  scs_grp_composition_rel.delete(A_rel_id_C, scs_user.system);
  scs_test.testBool('Check Representation', 2, scs_group.check_representation_all = 't') ;

  scs_test.testBool('Memberships', 7, 
  /* bob DM C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  /* bob  M C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  /* bob DM A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* bob  M A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 'f' AND
  /* bob DM B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* bob  M B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 'f');

  scs_test.testBool('Memberships', 8, 
  /* jane  M C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  /* jane DM C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  /* jane  M D(t) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 't' AND
  /* jane DM D(t) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 't' AND
  /* jane  M B(t) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 't' AND
  /* jane DM B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* jane  M A(t) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 't' AND
  /* jane DM A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  scs_test.printl('[Remove D<--jane membership and check membership for jane]');
  scs_grp_member_rel.delete(D_rel_id_jane, scs_user.system);
  scs_test.testBool('Check Representation', 3, scs_group.check_representation_all = 't') ;

  scs_test.testBool('Memberships', 9, 
  /* jane  M C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  /* jane DM C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  /* jane  M D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 'f' AND
  /* jane DM D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 'f' AND
  /* jane  M B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 'f' AND
  /* jane DM B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* jane  M A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 'f' AND
  /* jane DM A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  scs_test.printl('[Add B<--C membership and check membership for jane and bob]');
  B_rel_id_C_member := scs_grp_member_rel.new(grp_id => B, party_id => C, modifying_user => scs_user.system);
  scs_test.testBool('Check Representation', 4, scs_group.check_representation_all = 't') ;

  scs_test.testBool('Memberships', 10, 
  /* bob DM C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  /* bob  M C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  /* bob DM A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* bob  M A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 'f' AND
  /* bob DM B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* bob  M B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 'f');

  scs_test.testBool('Memberships', 11, 
  /* jane  M C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  /* jane DM C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  /* jane  M D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 'f' AND
  /* jane DM D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 'f' AND
  /* jane  M B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 'f' AND
  /* jane DM B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* jane  M A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 'f' AND
  /* jane DM A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  scs_test.printl('[Delete B<--C membership]');
  scs_grp_member_rel.delete(B_rel_id_C_member,scs_user.system);
  scs_test.testBool('Check Representation', 5, scs_group.check_representation_all = 't') ;

  scs_test.printl('[Add B<--C composition and check membership for jane, bob and betty]');
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => C, modifying_user => scs_user.system);
  scs_test.testBool('Check Representation', 6, scs_group.check_representation_all = 't') ;

  scs_test.testBool('Memberships', 12, 
  /* bob DM C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  /* bob  M C(t) */ scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  /* bob DM A(f) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* bob  M A(t) */ scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 't' AND
  /* bob DM B(f) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* bob  M B(t) */ scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 't');

  scs_test.testBool('Memberships', 13, 
  /* jane  M C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  /* jane DM C(t) */ scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  /* jane  M D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 'f' AND
  /* jane DM D(f) */ scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 'f' AND
  /* jane  M B(t) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 't' AND
  /* jane DM B(f) */ scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* jane  M A(t) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 't' AND
  /* jane DM A(f) */ scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  scs_test.testBool('Memberships', 14, 
  /* betty DM E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f') = 't' AND
  /* betty  M E(t) */ scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t') = 't' AND
  /* betty DM C(f) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f') = 'f' AND
  /* betty  M C(t) */ scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t') = 't' AND
  /* betty DM B(f) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f') = 'f' AND
  /* betty  M B(t) */ scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t') = 't' AND
  /* betty DM A(f) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f') = 'f' AND
  /* betty  M A(t) */ scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t') = 't');

  scs_test.print('[Remove the test groups...');
  scs_group.delete(G);
  scs_group.delete(F);
  scs_group.delete(E);
  scs_group.delete(D);
  scs_group.delete(C);
  scs_group.delete(B);
  scs_group.delete(A);
  scs_test.printl(']');

  scs_test.print('[Remove the test members...');
  scs_user.delete(joe);
  scs_user.delete(jane);
  scs_user.delete(bob);
  scs_user.delete(betty);
  scs_user.delete(jack);
  scs_user.delete(jill);
  scs_user.delete(sven);
  scs_user.delete(stacy);
  scs_test.printl(']');

  scs_test.testBool('Check Representation', 7, scs_group.check_representation_all = 't') ;

  scs_test.printl('[Create new users and groups to test fail on direct composition cycles]');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => A, modifying_user => scs_user.system);
  scs_test.testExn('DC cycle', 1, 
    'declare
       rel_id integer;
     begin
       rel_id := scs_grp_composition_rel.new(grp_id_one => ' || A || ', 
                                             grp_id_two => ' || B || ', 
                                             modifying_user => scs_user.system);
     end;','t');
  scs_group.delete(B);
  scs_group.delete(A);

  scs_test.printl('[Test fail on indirect composition cycles]');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  C := scs_group.new(grp_name => 'C', email => 'C', modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => A, grp_id_two => B, modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => C, modifying_user => scs_user.system);
  print_scs_grp_party_index;
  scs_test.testExn('C cycle', 1,
    'declare
       rel_id integer;
     begin
       rel_id := scs_grp_composition_rel.new(grp_id_one => ' || C || ', 
                                             grp_id_two => ' || A || ',
                                             modifying_user => scs_user.system);
     end;','t');

  scs_group.delete(C);
  scs_group.delete(B);
  scs_group.delete(A);
end;
/
show errors


-- If you have errors, then look in the scs-log table.
-- select message
--   from scs_logs;

