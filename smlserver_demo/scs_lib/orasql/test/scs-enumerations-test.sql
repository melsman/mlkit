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
  scs_test.testBool( 'updateValue', 1, 
	             textid1 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );

  textid2 := scs_text.new;
  valid2 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid2 );
  scs_test.testBool( 'updateValue', 2, valid1 = valid2 );

  valid2 := scs_enumeration.updateValue( enum_id => enumid1, 
					 value => 'kandidat', 
					 text_id => textid2 );
  scs_test.testBool( 'updateValue', 3, 
	             textid2 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );
end;
/