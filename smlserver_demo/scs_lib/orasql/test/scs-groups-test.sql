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

create or replace procedure print (
  s varchar
)
is
begin
  dbms_output.put( s );
end print;
/
show errors

create or replace procedure printl (
  s varchar2
)
is
begin
  dbms_output.put_line( s );
end printl;
/
show errors

create or replace procedure print_scs_grp_party_index 
is
begin
  scs_log.notice('GT','| Group |      Party | Rel_id | Container | Ancestor |');
  for row in (select scs_group.name(grp_id) as grp, 
                     party.email(party_id) as party, 
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

create or replace procedure check_rep
is
begin
  print('[check_rep...');
  for g in (select * 
              from scs_groups) loop
    if scs_group.check_representation(g.grp_id) = 'f' then
      printl('Group ' || g.grp_name || ' (' || g.grp_id || ') failed.');
    end if;
  end loop;
  printl(']');
end check_rep;
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
  print('[Create the test groups...');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  C := scs_group.new(grp_name => 'C', email => 'C', modifying_user => scs_user.system);
  D := scs_group.new(grp_name => 'D', email => 'D', modifying_user => scs_user.system);
  E := scs_group.new(grp_name => 'E', email => 'E', modifying_user => scs_user.system);
  F := scs_group.new(grp_name => 'F', email => 'F', modifying_user => scs_user.system);
  G := scs_group.new(grp_name => 'G', email => 'G', modifying_user => scs_user.system);
  printl(']');

  -- Create the test members.
  print('[Create users...');
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
  printl(']');

  delete from scs_logs;

  print('[Build member and composition relations (se test case in the documentation)...');
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
  printl(']');

  select count(*) 
    into n_rows
    from scs_grp_party_index;
  printl('There are ' || n_rows || ' rows in scs_grp_party_index (we expect 25 rows).');

  printl('Testing memberships');
  printl('  DC = Direct Component of');
  printl('   C = Component of');
  printl('  DM = Direct Member of');
  printl('   M = Member of');

--   printl('joe DM D(t): ' || scs_group.member_p(party_id => joe, grp_id => D, cascade_membership=>'f'));
--   printl('joe DM B(f): ' || scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'f'));
--   printl('joe  M B(t): ' || scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'t'));
--   printl('joe DM A(f): ' || scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'f'));
--   printl('joe  M A(t): ' || scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'t'));

--   printl('jane  M C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t'));
--   printl('jane DM C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f'));
--   printl('jane  M D(t): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t'));
--   printl('jane DM D(t): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f'));
--   printl('jane  M B(t): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t'));
--   printl('jane DM B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f'));
--   printl('jane  M A(t): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t'));
--   printl('jane DM A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f'));

--   printl('bob DM C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f'));
--   printl('bob  M C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t'));
--   printl('bob DM A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f'));
--   printl('bob  M A(t): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t'));
--   printl('bob DM B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f'));
--   printl('bob  M B(t): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t'));

--   printl('betty DM E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f'));
--   printl('betty  M E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t'));
--   printl('betty DM C(f): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f'));
--   printl('betty  M C(t): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t'));
--   printl('betty DM B(f): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f'));
--   printl('betty  M B(t): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t'));
--   printl('betty DM A(f): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f'));
--   printl('betty  M A(t): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t'));

  printl('Remove B<--C composition and check membership for bob and betty');
  scs_grp_composition_rel.delete(B_rel_id_C, scs_user.system);
  check_rep;

--   printl('bob DM C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f'));
--   printl('bob  M C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t'));
--   printl('bob DM A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f'));
--   printl('bob  M A(t): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t'));
--   printl('bob DM B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f'));
--   printl('bob  M B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t'));

--   printl('betty DM E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f'));
--   printl('betty  M E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t'));
--   printl('betty DM C(f): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f'));
--   printl('betty  M C(t): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t'));
--   printl('betty DM B(f): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f'));
--   printl('betty  M B(f): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t'));
--   printl('betty DM A(f): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f'));
--   printl('betty  M A(t): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t'));

  printl('Remove A<--C composition and check membership for bob and jane');
  scs_grp_composition_rel.delete(A_rel_id_C, scs_user.system);
  check_rep;

--   printl('bob DM C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f'));
--   printl('bob  M C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t'));
--   printl('bob DM A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f'));
--   printl('bob  M A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t'));
--   printl('bob DM B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f'));
--   printl('bob  M B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t'));

--   printl('jane  M C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t'));
--   printl('jane DM C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f'));
--   printl('jane  M D(t): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t'));
--   printl('jane DM D(t): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f'));
--   printl('jane  M B(t): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t'));
--   printl('jane DM B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f'));
--   printl('jane  M A(t): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t'));
--   printl('jane DM A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f'));

  printl('Remove D<--jane membership and check membership for jane');
  scs_grp_member_rel.delete(D_rel_id_jane, scs_user.system);
  check_rep;

  printl('jane  M C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t'));
  printl('jane DM C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f'));
  printl('jane  M D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t'));
  printl('jane DM D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f'));
  printl('jane  M B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t'));
  printl('jane DM B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f'));
  printl('jane  M A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t'));
  printl('jane DM A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f'));

  printl('Add B<--C membership and check membership for jane and bob');
  B_rel_id_C_member := scs_grp_member_rel.new(grp_id => B, party_id => C, modifying_user => scs_user.system);
  check_rep;

  printl('bob DM C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f'));
  printl('bob  M C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t'));
  printl('bob DM A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f'));
  printl('bob  M A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t'));
  printl('bob DM B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f'));
  printl('bob  M B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t'));

  printl('jane  M C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t'));
  printl('jane DM C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f'));
  printl('jane  M D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t'));
  printl('jane DM D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f'));
  printl('jane  M B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t'));
  printl('jane DM B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f'));
  printl('jane  M A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t'));
  printl('jane DM A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f'));


  printl('Delete B<--C membership');
  scs_grp_member_rel.delete(B_rel_id_C_member,scs_user.system);
  check_rep;

  printl('Add B<--C composition and check membership for jane, bob and betty');
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => C, modifying_user => scs_user.system);
  check_rep;

  printl('bob DM C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f'));
  printl('bob  M C(t): ' || scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t'));
  printl('bob DM A(f): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f'));
  printl('bob  M A(t): ' || scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t'));
  printl('bob DM B(f): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f'));
  printl('bob  M B(t): ' || scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t'));

  printl('jane  M C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t'));
  printl('jane DM C(t): ' || scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f'));
  printl('jane  M D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t'));
  printl('jane DM D(f): ' || scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f'));
  printl('jane  M B(t): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t'));
  printl('jane DM B(f): ' || scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f'));
  printl('jane  M A(t): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t'));
  printl('jane DM A(f): ' || scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f'));

  printl('betty DM E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f'));
  printl('betty  M E(t): ' || scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t'));
  printl('betty DM C(f): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f'));
  printl('betty  M C(t): ' || scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t'));
  printl('betty DM B(f): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f'));
  printl('betty  M B(t): ' || scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t'));
  printl('betty DM A(f): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f'));
  printl('betty  M A(t): ' || scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t'));


  print('[Remove the test groups...');
  scs_group.delete(G);
  scs_group.delete(F);
  scs_group.delete(E);
  scs_group.delete(D);
  scs_group.delete(C);
  scs_group.delete(B);
  scs_group.delete(A);
  printl(']');

  print('[Remove the test members...');
  scs_user.delete(joe);
  scs_user.delete(jane);
  scs_user.delete(bob);
  scs_user.delete(betty);
  scs_user.delete(jack);
  scs_user.delete(jill);
  scs_user.delete(sven);
  scs_user.delete(stacy);
  printl(']');

  check_rep;

  printl('Create new users and groups to test fail on direct composition cycles');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => A, modifying_user => scs_user.system);
  -- the next composition must fail.
  --rel_id := scs_grp_composition_rel.new(grp_id_one => A, grp_id_two => B, modifying_user => scs_user.system);
  scs_group.delete(B);
  scs_group.delete(A);

  printl('Test fail on indirect composition cycles');
  A := scs_group.new(grp_name => 'A', email => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', email => 'B', modifying_user => scs_user.system);
  C := scs_group.new(grp_name => 'C', email => 'C', modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => A, grp_id_two => B, modifying_user => scs_user.system);
  rel_id := scs_grp_composition_rel.new(grp_id_one => B, grp_id_two => C, modifying_user => scs_user.system);
  print_scs_grp_party_index;
  -- the next composition must fail
  -- rel_id := scs_grp_composition_rel.new(grp_id_one => C, grp_id_two => A, modifying_user => scs_user.system);

  scs_group.delete(C);
  scs_group.delete(B);
  scs_group.delete(A);
end;
/
show errors

select message
  from scs_logs;

