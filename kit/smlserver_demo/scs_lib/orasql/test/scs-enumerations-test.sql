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
  enumid1 := scs_enumeration.new( name => 'graduate_programmes' );
  enumid2 := scs_enumeration.new( enum_id => 2, name => 'graduate_programmes2' );
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

  -- Testing getTID
  scs_test.testExn('getTID', 1,
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
                       scs_enumeration.destroy(name => ''graduate_programmes2'');
                     end;' );

  -- destroy name that does not exists - this should not fail
  scs_test.testUnit('destroy', 4,
                    'begin 
                       scs_enumeration.destroy(name => ''graduate_programmes2'');  
                     end;');

end;
/

