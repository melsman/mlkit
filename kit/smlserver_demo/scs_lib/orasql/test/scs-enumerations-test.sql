declare
	enumid1 integer;
	enumid2 integer;

	textid1 integer;
	textid2 integer;
begin
	enumid1 := scs_enumeration.new( name => 'graduate_programmes' );
	enumid2 := scs_enumeration.new( enum_id => 2, name => 'graduate_programmes2' );
	scs_test.printTest( 'new', 1, enumid2 = 2 );

	textid1 := scs_text.new;
	scs_enumeration.updateValue( enum_id => enumid1, 
				  value => 'kandidat', 
				  text_id => textid1 );
	scs_test.printTest( 'updateValue', 1, 
			    textid1 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );
	textid2 := scs_text.new;
	scs_enumeration.updateValue( enum_id => enumid1, 
				  value => 'kandidat', 
				  text_id => textid2 );
	scs_test.printTest( 'updateValue', 2, 
			    textid2 = scs_enumeration.getTID( enum_id => enumid1, value => 'kandidat' ) );


end;
/