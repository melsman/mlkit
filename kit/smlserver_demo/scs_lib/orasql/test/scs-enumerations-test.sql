/* ======================================================================
   test suite for scs_enumeration package

   $Id$

   History: 
   271102 Kennie Nybo Pontoppidan <kennie@it-c.dk> 
	  code review, added test cases and comments
   161002 Niels Hallenberg <nh@it.edu> created test suite
====================================================================== */

set serveroutput on;

declare
  enumid1	integer;
  enumid2	integer;
  id		integer;
  rand_name	varchar2(10);
  rand_name2	varchar2(10);
  rand_name3	varchar2(10);
  textid1	integer;
  textid2	integer;

  valid1	integer;
  valid2	integer;
  valid3	integer;

  illegal_id	integer;

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
  clean_up	exception;
begin
  scs_test.printl( '-------------------------------' );
  scs_test.printl( 'testing scs_enumeration package' );
  scs_test.printl( '-------------------------------' );

  select count(*) into counter0_b from scs_enumerations;
  select count(*) into counter00_b from scs_enum_values;
  select count(*) into counter000_b from scs_texts;
  select count(*) into counter0000_b from scs_text_lang;

  illegal_id := scs.new_obj_id;

  -- testing new
  scs_test.printl( 'testing function ''new'':' );

  rand_name := scs_random.rand_string(10);
  scs_test.testUnit( 'new', 1, '
    declare 
      eid integer;
    begin
      eid := scs_enumeration.new( name => '''|| rand_name || ''' );
    end;' );
  select enum_id into enumid1 from scs_enumerations where name = rand_name;

  id := scs.new_obj_id;
  rand_name2 := scs_random.rand_string(10);
  enumid2 := scs_enumeration.new( enum_id => id, name =>  rand_name2 );
  scs_test.testBool( 'new', 2, enumid2 = id );

  -- duplicate enum_id
  scs_test.testExn( 'new', 3, '
    declare
      eid integer;
    begin
      eid := scs_enumeration.new( enum_id => ' || enumid1 || ', 
				  name => scs_random.rand_string(10) );
    end;', 'f' );

  -- duplicate name
  scs_test.testExn( 'new', 4, '
    declare
      eid integer;
    begin
      eid := scs_enumeration.new( name => '''|| rand_name ||''' );
    end;', 'f' );  

  -- testing exists_p
  scs_test.printl( 'testing function ''exists_p'':' );
  scs_test.testBool( 'exists_p', 1, 
    scs_enumeration.exists_p( enumid1 ) = 't' );
  scs_test.testBool( 'exists_p', 2, 
    scs_enumeration.exists_p( illegal_id ) = 'f' );
  
  -- testing updateValue, getTID
  scs_test.printl( 'testing functions ''updateValue'' and ''getTID'':' );
  textid1 := scs_text.new;
  -- new (enum_id, value) pair
  valid1 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid1 );
  scs_test.testBool('updateValue', 1, 
    textid1 = scs_enumeration.getTID( enum_id => enumid1, 
				      value => 'kandidat') );
  scs_test.testBool('getTID, 2 parameter version', 1, 
    textid1 = scs_enumeration.getTID( enum_id => enumid1, 
				      value => 'kandidat') );

  scs_test.testBool('getTID, 1 parameter version', 1, 
    textid1 = scs_enumeration.getTID( val_id => valid1 ) );

  -- update existing (enum_id, value) pair
  textid2 := scs_text.new;
  valid2 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid2 );
  scs_test.testBool('updateValue', 2, valid1 = valid2 );
  scs_test.testBool('updateValue', 3, 
    textid2 = scs_enumeration.getTID( enum_id => enumid1, 
				      value => 'kandidat') );
  -- check that textid1 was actually deleted from scs_texts.
  -- and that textid2 is still there
  scs_test.testBool('updateValue', 4, scs_text.exists_p(textid1) = 'f');
  scs_test.testBool('updateValue', 5, scs_text.exists_p(textid2) = 't');

  -- multiple (1000) updates on (enumid1, 'kandidat')
  scs_test.testUnit( 'updateValue', 6, '
    declare
      tid	integer;
      vid	integer;
      i		integer;
    begin
      for i in 1..1000 loop
        tid := scs_text.new;
        vid := scs_enumeration.updateValue( 
	         enum_id => ' || enumid1 || ', 
	         value => ''kandidat'', 
	         text_id => tid );
      end loop;
    end;' );

  -- unknown enum_id
  scs_test.testExn( 'updateValue', 7, '
    declare
      vid	integer;
    begin
      vid := scs_enumeration.updateValue( enum_id => '|| illegal_id || ',
					  value => ''xxxx'',
					  text_id => '|| textid2 ||' );
    end;', 'f' );

  -- unknown text_id
  scs_test.testExn( 'updateValue', 8, '
    declare
      vid	integer;
    begin
      vid := scs_enumeration.updateValue( enum_id => '|| enumid1 || ',
					  value => ''xxxx'',
					  text_id => '|| illegal_id ||' );
    end;', 'f' );
  

  -- getTID: unknown value
  scs_test.testExn('getTID, 2 parameter version', 2, '
    declare
      textid integer;
    begin
      textid := scs_enumeration.getTID( enum_id => ' || enumid1 || ', 
					value => ''Does not exists...'');
    end;', 'f' );

  -- getTID, 2 parameter: null value returned
  rand_name3 := scs_random.rand_string(10);
  valid3 := scs.new_obj_id;
  insert into scs_enum_values (val_id, enum_id, value, active_p, ordering) 
  values( valid3, enumid1, rand_name3, 'f', 100 );
  scs_test.testBool( 'getTID, 2 parameter version', 3,
    scs_enumeration.getTID( enum_id => enumid1, 
			    value => rand_name3 ) is null );

  -- getTID, 1 parameter: null value returned
  scs_test.testBool( 'getTID, 2 parameter version', 2,
    scs_enumeration.getTID( val_id => valid3 ) is null );

  -- getTID, 1 parameter version: unknown val_id
  scs_test.testExn('getTID, 1 parameter version', 3, '
    declare
      textid integer;
    begin
      textid := scs_enumeration.getTID( val_id => -1 );
    end;', 'f' );

  -- testing getVID
  scs_test.printl( 'testing function ''getVID'':' );
  valid1 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'new_val', 
					 text_id => scs_text.new );
  -- known val_id
  scs_test.testBool( 'getVID', 1, 
    scs_enumeration.getVID( enum_id => enumid1, 
			    value => 'new_val') = valid1 );
  -- unknown enum_id
  scs_test.testBool( 'getVID', 2, 
    scs_enumeration.getVID( enum_id => illegal_id, 
			    value => 'new_val') is null );
  -- unknown value
  scs_test.testBool( 'getVID', 3, 
    scs_enumeration.getVID( enum_id => enumid1, 
			    value => scs_random.rand_string(10) ) is null );
  -- unknown enum_id, unknown value
  scs_test.testBool( 'getVID', 4, 
    scs_enumeration.getVID( enum_id => illegal_id,
			    value => scs_random.rand_string(10) ) is null );

  scs_test.printl( 'testing procedures ''destroy'':' );
  -- unknown enum_id
  scs_test.testUnit('destroy, enum_id version', 1, '
    begin 
      scs_enumeration.destroy (enum_id => ' || illegal_id || ');
    end;' );

  select count(*) into counter1_b from scs_enumerations;
  -- destroy enum_id that exists
  scs_test.testUnit('destroy, enum_id version', 2, '
    begin
      scs_enumeration.destroy (enum_id => ' || enumid1 || ');
    end;' );
  select count(*) into counter1_a from scs_enumerations;
  scs_test.testBool( 'destroy, enum_id version', 3, counter1_a = counter1_b -1 );

  -- destroy name that exists
  select count(*) into counter1_b from scs_enumerations;
  scs_test.testUnit('destroy, name version', 1,
                    'begin
                       scs_enumeration.destroy(name => ''' || rand_name2 || ''');
                     end;' );
  select count(*) into counter1_a from scs_enumerations;
  scs_test.testBool( 'destroy, name version', 2, counter1_a = counter1_b -1 );

  -- destroy name that does not exists - this should not fail
  scs_test.testUnit('destroy, name version', 3, '
    begin 
      scs_enumeration.destroy(name => scs_random.rand_string(10) );  
    end;' );

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when others then
  -- cleaning up
  scs_test.printl( 'cleaning up:' );

  delete scs_enum_values where enum_id in ( enumid1, enumid2 );
  delete scs_text_lang
   where text_id in ( select text_id from scs_enum_values 
		      where enum_id in ( enumid1, enumid2 ) );
  delete from scs_texts 
   where text_id in ( select text_id from scs_enum_values 
		      where enum_id in ( enumid1, enumid2 ) );
  delete from scs_enumerations where enum_id in ( enumid1, enumid2 );

  select count(*) into counter0_a from scs_enumerations;
  select count(*) into counter00_a from scs_enum_values;
  select count(*) into counter000_a from scs_texts;
  select count(*) into counter0000_a from scs_text_lang;

  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
  scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
  scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
end;
/


declare
  enumid1	integer;
  enumid2	integer;
  unique_name	varchar2(100);
  unique_name2	varchar2(100);
  illegal_id	integer;

  vid1		integer;
  vid2		integer;
  vid3		integer;
  vid4		integer;
  vid5		integer;
  tid		integer;
  eh_ordering1	integer;
  eh_ordering2	integer;
  dkm_ordering1 integer;
  dkm_ordering2 integer;

  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
  counter000_b	integer;
  counter000_a	integer;
  counter0000_b	integer;
  counter0000_a	integer;

  clean_up	exception;
begin
  select count(*) into counter0_b from scs_enumerations;
  select count(*) into counter00_b from scs_enum_values;
  select count(*) into counter000_b from scs_texts;
  select count(*) into counter0000_b from scs_text_lang;

  illegal_id := scs.new_obj_id;  
  unique_name := 'test' || scs.new_obj_id;
  enumid1 := scs_enumeration.new( name => unique_name );

  scs_test.printl( 'testing function ''getEnumId'':' );
  -- known name
  scs_test.testBool( 'getEnumId', 1, 
    scs_enumeration.getEnumID( name => unique_name ) = enumid1 );
  -- unknown name
  scs_test.testBool( 'getEnumId', 2,
    scs_enumeration.getEnumID( name => scs_random.rand_string(10) ) is null );

  scs_test.printl( 'testing function ''getName'':' );
  -- known enum_id
  scs_test.testBool( 'getName', 1, 
    scs_enumeration.getName( enum_id => enumid1 ) = unique_name );
  -- unknown enum_id
  scs_test.testBool( 'getName', 2,
    scs_enumeration.getName( enum_id => illegal_id ) is null );

  -- testing addValue
  scs_test.printl( 'testing function ''addValue'':' );
  vid1 := scs_enumeration.addValue ( unique_name, 'dkmtest', 'da', 'design');
  vid2 := scs_enumeration.addValue ( unique_name, 'ehtest', 'da', 'design2');
  -- 2 entries added?
  scs_test.testBool( 'addValue', 1, 
    scs_enumeration.getVID( enum_id => enumid1, value => 'dkmtest' ) = vid1 AND
    scs_enumeration.getVID( enum_id => enumid1, value => 'ehtest' ) = vid2 );
  -- correct texts?
  scs_test.testBool( 'addValue', 2, 
    scs_text.getText( text_id => scs_enumeration.getTID(vid1),
		      language => 'da' ) = 'design' AND
    scs_text.getText( text_id => scs_enumeration.getTID(vid2),
		      language => 'da' ) = 'design2' );
  
  -- name is unknown
  scs_test.testExn( 'addValue', 3, '
    declare
      vid integer;
    begin
      vid := scs_enumeration.addValue(
	       name	=> scs_random.rand_string(10),
	       value	=> ''kandidat'',
	       language => ''da'',
	       text	=> ''hest'' );
    end;', 'f' );
  -- value is null
  scs_test.testExn( 'addValue', 4, '
    declare
      vid integer;
    begin
      vid := scs_enumeration.addValue(
	       name	=> ''' || unique_name || ''',
	       value	=> null,
	       language => ''da'',
	       text	=> ''hest'' );
    end;', 'f' );
  -- (enum_id, value) pair exists
  scs_test.testExn( 'addValue', 5, '
    declare
      vid integer;
    begin
      vid := scs_enumeration.addValue(
	       name	=> ''' || unique_name || ''',
	       value	=> ''dkmtest'',
	       language => ''da'',
	       text	=> ''hest'' );
    end;', 'f' );
  -- language is unknown
  scs_test.testExn( 'addValue', 6, '
    declare
      vid integer;
    begin
      vid := scs_enumeration.addValue(
	       name	=> ''' || unique_name || ''',
	       value	=> ''new_value_dkm'',
	       language => ''xy'',
	       text	=> ''hest'' );
    end;', 'f' );

  scs_test.printl( 'testing function ''getVal'':' );
  -- known val_id
  scs_test.testBool( 'getVal', 1, 
    scs_enumeration.getVal( val_id => vid1 ) = 'dkmtest' );
  -- unknown val_id
  scs_test.testBool( 'getVal', 2,
    scs_enumeration.getVal( val_id => illegal_id ) is null );

  scs_test.printl( 'testing procedure ''swap_ordering'':' );
  vid1 := scs_enumeration.addValue ( unique_name, 'test_swap', 'da', 'design');
  vid2 := scs_enumeration.addValue ( unique_name, 'test_swap2', 'da', 'design2');

  -- legal values of val_id1 and val_id2
  select ordering into dkm_ordering1 
    from scs_enum_values 
   where value = 'test_swap' 
     and enum_id = enumid1;

  select ordering into eh_ordering1 
    from scs_enum_values 
   where value = 'test_swap2'
     and enum_id = enumid1;
  scs_enumeration.swapOrdering( vid1, vid2);
  select ordering into dkm_ordering2 from scs_enum_values where value = 'test_swap';
  select ordering into eh_ordering2 from scs_enum_values where value = 'test_swap2';
  scs_test.testBool( 'swapOrdering', 1,
    eh_ordering2 = dkm_ordering1 and dkm_ordering2 = eh_ordering1 );
  
  -- unknown val_id1
  scs_test.testExn( 'swapOrdering', 2, '
    begin
      scs_enumeration.swapOrdering( '|| illegal_id ||', '|| vid2 ||');
    end;', 'f' );

  -- unknown val_id2
  scs_test.testExn( 'swapOrdering', 3, '
    begin
      scs_enumeration.swapOrdering( '|| vid1 ||', '|| illegal_id ||');
    end;', 'f' );

  -- unknown val_id1, val_id2
  scs_test.testExn( 'swapOrdering', 4, '
    begin
      scs_enumeration.swapOrdering( '|| illegal_id ||', '|| illegal_id ||');
    end;', 'f' );

  -- val_ids from different enumerations
  unique_name2 := 'test' || scs.new_obj_id;
  enumid2 := scs_enumeration.new( name => unique_name2 );
  vid3 := scs_enumeration.addValue ( unique_name2, 'test_swap3', 'da', 'design2');
  scs_test.testExn( 'swapOrdering', 2, '
    begin
      scs_enumeration.swapOrdering( '|| vid1 ||', '|| vid3 ||');
    end;', 'f' );

  -- testing vidToString
  scs_test.printl( 'testing function ''vidToString'':' );
  tid := scs_text.new;
  tid := scs_text.updateText( 
    text_id	=> tid,	
    language	=> 'da',
    text	=> 'dansk',
    language2	=> 'en',
    text2	=> 'english' );
  vid4 := scs_enumeration.updateValue( 
    enum_id => enumid1,
    text_id => tid,
    value   => 'test_vidToString' );
  -- legal values, danish
  scs_test.testBool( 'vidToString', 1, 
    scs_enumeration.vidToString( val_id => vid4, language => 'da') = 'dansk' );

  -- legal values, english
  scs_test.testBool( 'vidToString', 2,
    scs_enumeration.vidToString( val_id => vid4, language => 'en') = 'english' );

  -- legal values, null
  tid := scs_text.new;
  tid := scs_text.updateText( 
    text_id	=> tid,	
    language	=> 'da',
    text	=> null );
  vid5 := scs_enumeration.updateValue( 
    enum_id => enumid1,
    text_id => tid,
    value   => 'test_vidToString_null' );
  scs_test.testBool( 'vidToString', 3,
    scs_enumeration.vidToString( val_id => vid5, language => 'da') is null );

  -- illegal values: unknown val_id
  scs_test.testExn( 'vidToString', 4, '
    declare
      text	scs_text_lang.text%TYPE;
    begin
      text := scs_enumeration.vidToString( 
	        val_id => '|| illegal_id ||', 
		language => ''da'' );
    end;', 'f' );

  -- illegal values: illegal language
  scs_test.testExn( 'vidToString', 5, '
    declare
      text	scs_text_lang.text%TYPE;
    begin
      text := scs_enumeration.vidToString( 
	        val_id => '|| vid5 ||', 
		language => ''xx'' );
    end;', 'f' );

  -- no text on language
  scs_test.testExn( 'vidToString', 6, '
    declare
      text	scs_text_lang.text%TYPE;
    begin
      text := scs_enumeration.vidToString( 
	        val_id => '|| vid5 ||', 
		language => ''en'' );
    end;', 'f' );
  
  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when others then 
  -- cleaning up
  scs_test.printl( 'cleaning up:' );

  delete scs_enum_values where enum_id in ( enumid1, enumid2 );
  delete scs_text_lang
   where text_id in ( select text_id from scs_enum_values 
		      where enum_id in ( enumid1, enumid2 ) );
  delete from scs_texts 
   where text_id in ( select text_id from scs_enum_values 
		      where enum_id in ( enumid1, enumid2 ) );
  delete from scs_enumerations where enum_id in ( enumid1, enumid2 );

  select count(*) into counter0_a from scs_enumerations;
  select count(*) into counter00_a from scs_enum_values;
  select count(*) into counter000_a from scs_texts;
  select count(*) into counter0000_a from scs_text_lang;

  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
  scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
  scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
end;
/
show errors







