-- test suite for scs_enumeration package
-- $Id$

declare
  enumid1 integer;
  enumid2 integer;

  textid1 integer;
  textid2 integer;

  valid1 integer;
  valid2 integer;
begin
  scs_test.printl('Testing Enumerations...');

  -- testing new
  enumid1 := scs_enumeration.new( name => 'test_programmes' );
  enumid2 := scs_enumeration.new( enum_id => 2, name => 'test_programmes2' );
  scs_test.testBool( 'new', 1, enumid2 = 2 );

  -- testing updateValue
  textid1 := scs_text.new;
  valid1 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid1 );
  scs_test.testBool('updateValue', 1, 
	            textid1 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );

  textid2 := scs_text.new;
  valid2 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid2 );
  scs_test.testBool('updateValue', 2, valid1 = valid2 );

  scs_test.testBool('updateValue', 3, scs_text.exists_p(textid2) = 't');

  valid2 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid2 );
  scs_test.testBool('updateValue', 4, 
	            textid2 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );

  -- Check that textid1 is actually deleted from scs_texts.
  scs_test.testBool('updateValue', 5, scs_text.exists_p(textid1) = 'f');
  scs_test.testBool('updateValue', 6, scs_text.exists_p(textid2) = 't');

  -- Testing getTID, 2 parameter version
  scs_test.testExn('getTID, 2 parameter version', 1,
                   'declare
                      textid integer;
                    begin
                      textid := scs_enumeration.getTID(enum_id => ' || enumid1 || ', value => ''Does not exists...'');
                    end;','f');


  -- enum_id 42 does not exist, but it should not fail.
  scs_test.testUnit('destroy', 1,
                    'begin 
                       scs_enumeration.destroy (enum_id => 42);
                     end;');

  -- destroy enum_id that exists
  scs_test.testUnit('destroy', 2,
                    'begin
                       scs_enumeration.destroy (enum_id => ' || enumid1 || ');
                     end;');

  -- destroy name that exists
  scs_test.testUnit('destroy', 3,
                    'begin
                       scs_enumeration.destroy(name => ''test_programmes2'');
                     end;' );

  -- destroy name that does not exists - this should not fail
  scs_test.testUnit('destroy', 4,
                    'begin 
                       scs_enumeration.destroy(name => ''test_programmes2'');  
                     end;');

end;
/

declare
  eid integer;
  vid1 integer;
  vid2 integer;
  vid3 integer;
  tid integer;
  eh_ordering1 integer;
  eh_ordering2 integer;
  dkm_ordering1 integer;
  dkm_ordering2 integer;


begin
  eid := scs_enumeration.new( name => 'test' );

  vid1 := scs_enumeration.addValue ( 'test', 'dkmtest', 'da', 'design');
  vid2 := scs_enumeration.addValue ( 'test', 'ehtest', 'da', 'design');

  select ordering into dkm_ordering1 from scs_enum_values where value = 'dkmtest';
  select ordering into eh_ordering1 from scs_enum_values where value = 'ehtest';
  scs_enumeration.swapOrdering( vid1, vid2);
  select ordering into dkm_ordering2 from scs_enum_values where value = 'dkmtest';
  select ordering into eh_ordering2 from scs_enum_values where value = 'ehtest';
  scs_test.testBool( 'swapOrdering', 1,
    eh_ordering2 = dkm_ordering1 and dkm_ordering2 = eh_ordering1 );

  -- Testing getTID, 1 parameter version
  scs_test.testExn('getTID, 1 parameter version', 1,
                   'declare
                      textid integer;
                    begin
                      textid := scs_enumeration.getTID( val_id => -1 );
                    end;','f');

  tid := scs_text.new;
  vid3 := scs_enumeration.updateValue( enum_id => eid, 
					 value => 'hest', 
					 text_id => tid );
  scs_test.testBool('getTID, 1 parameter version', 1, 
	            scs_enumeration.getTID( val_id => vid3 ) = tid );
  
  -- cleaning up
  scs_enumeration.destroy( 'test' );
  
end;
/
show errors

