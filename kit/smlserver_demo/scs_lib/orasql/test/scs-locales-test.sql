/*
test of scs_text package
$Id$
*/

set serveroutput on
declare
  tid1 integer;
  tid2 integer;
  tid3 integer;
  tid4 integer;
  tmp integer;

  bool1 boolean;

begin
  scs_test.printl( 'testing module scs_text...');
  tid1 := scs_text.new;
  scs_test.testBool( 'new', 1, (tid1 is null) = false );
  tid2 := scs_text.new ( tid1 );
  scs_test.testBool( 'new', 2, tid1 = tid2 );

  tmp  := scs_text.updateText( tid1, 'da', 'kurt på dansk' );
  scs_test.testBool( 'updateText', 1, tmp = tid1 );
  bool1 := scs_text.getText( tid1, 'da' ) = 'kurt på dansk';
  scs_test.testBool( 'updateText', 2, bool1 );
  tmp  := scs_text.updateText( tid1, 'da', 'curt på dansk' );
  scs_test.testBool( 'updateText', 3, scs_text.getText( tid1, 'da' ) = 'curt på dansk' );
  tid3 := scs_text.updateText( language => 'da', text => 'niels på dansk',
			       language2 => 'en', text2 => 'niels in english' );  
  scs_test.testBool( 'updateText', 4, 
                      scs_text.getText( tid3, 'da' ) = 'niels på dansk' 
		      AND scs_text.getText( tid3, 'en' ) = 'niels in english' );
  tid4 := scs_text.updateText( language => 'da', text => 'sven på dansk' );
  scs_test.testBool( 'updateText', 5, scs_text.getText( tid4, 'da' ) = 'sven på dansk' ); 


  scs_test.testExn('updateText', 6, 'declare 
                                        tmp integer; 
                                      begin 
                                        tmp := scs_text.updateText( language => ''kd'', text => ''fejl på kd'');
                                      end;','f');

  scs_test.testExn('getText', 1, 'declare
                                    tmpString varchar(1000);
                                  begin
                                    tmpString := scs_text.getText( ' || tid4 || ', ''en'' ); 
                                  end;','f');
end;
/
