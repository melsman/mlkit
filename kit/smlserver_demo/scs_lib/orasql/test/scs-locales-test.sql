-- $Id$


set serveroutput on

declare
  tid1 integer;
  tid2 integer;
  tid3 integer;
  tid4 integer;
  tid5 integer;
  tid6 integer;
  tid7 integer;
  tmp integer;

  bool1 boolean;

  counter1_b integer;
  counter1_a integer;
  counter2_b integer;
  counter2_a integer;
begin
  scs_test.printl( '------------------------' );
  scs_test.printl( 'testing package scs_text');
  scs_test.printl( '------------------------' );

  select count(*) into counter1_b from scs_texts;
  select count(*) into counter2_b from scs_text_lang;

  scs_test.printl( 'testing function new:' );
  tid1 := scs_text.new;
  scs_test.testBool( 'new', 1, (tid1 is null) = false );
  tid2 := scs_text.new ( tid1 );
  scs_test.testBool( 'new', 2, tid1 = tid2 );

  scs_test.printl( 'testing function updateText:' );
  
  scs_test.testBool( 'updateText', 1, 
		     tid1 = scs_text.updateText(tid1, 'da', 'kurt på dansk') );
  -- create text in 'da'
  scs_test.testBool( 'updateText', 2, 
		     scs_text.getText( tid1, 'da' ) = 'kurt på dansk' );
  -- update text in 'da'
  tmp  := scs_text.updateText( tid1, 'da', 'curt på dansk' );
  scs_test.testBool( 'updateText', 3, scs_text.getText( tid1, 'da' ) = 'curt på dansk' );
  -- text_id null (2 languages)
  tid3 := scs_text.updateText( language => 'da', text => 'niels på dansk',
			       language2 => 'en', text2 => 'niels in english' );  
  scs_test.testBool( 'updateText', 4, 
                      scs_text.getText( tid3, 'da' ) = 'niels på dansk' 
		      AND scs_text.getText( tid3, 'en' ) = 'niels in english' );
  -- text_id null (1 language)
  tid4 := scs_text.updateText( language => 'da', text => 'sven på dansk' );
  scs_test.testBool( 'updateText', 5, scs_text.getText( tid4, 'da' ) = 'sven på dansk' ); 
  -- illegal language
  scs_test.testExn('updateText', 6, '
    declare 
      tmp integer; 
    begin 
      tmp := scs_text.updateText( language => ''kd'', text => ''fejl på kd'');
    end;', 'f' );
  -- illegal language2
  scs_test.testExn('updateText', 7, '
    declare 
      tmp integer; 
    begin 
      tmp := scs_text.updateText( language => ''da'', text => ''ok'', 
				  language2 => ''kk'', text2 => ''not ok'');
    end;', 'f' );

  scs_test.printl( 'testing function exists_p:' );
  scs_test.testBool('exists_p', 1, scs_text.exists_p(tid3) = 't');
  scs_test.testBool('exists_p', 2, scs_text.exists_p(tid3, 'da') = 't');
  scs_test.testBool('exists_p', 3, scs_text.exists_p(tid3, 'kd') = 'f');
  scs_test.testBool('exists_p', 4, scs_text.exists_p(42) = 'f');
  scs_test.testBool('exists_p', 5, scs_text.exists_p(42,'en') = 'f');
  tid5 := scs_text.new;
  scs_test.testBool('exists_p', 6, scs_text.exists_p(tid5) = 't');
  scs_test.testBool('exists_p', 7, scs_text.exists_p(tid5, 'da') = 'f');

  scs_test.printl( 'testing function getText:' );
  scs_test.testExn('getText', 1, '
    declare
      tmpString varchar(1000);
    begin
      tmpString := scs_text.getText( ' || tid4 || ', ''en'' ); 
    end;', 'f' );
  scs_test.testBool( 'getText', 2, 
    scs_text.getText( tid4, 'da' ) = 'sven på dansk' ); 
 
  scs_test.printl( 'testing function destroy:' );
  scs_test.printl( '(only accept test cases if exists_p passed tests)' );
  tid6 := scs_text.new;
  scs_text.destroy( text_id => tid6 );
  scs_test.testBool(  'destroy', 1, scs_text.exists_p( tid6 ) = 'f' );
  tid6 := scs_text.updateText( language => 'da', text => 'niels på dansk',
			       language2 => 'en', text2 => 'niels in english' );    
  -- leave one, destroy one
  scs_text.destroy( text_id => tid6, language => 'da' );
  scs_test.testBool(  'destroy', 2, scs_text.exists_p( tid6, 'da' ) = 'f' 
				AND scs_text.exists_p( tid6, 'en' ) = 't');
  -- destroy two
  tid7 := scs_text.updateText( language => 'da', text => 'niels på dansk',
			       language2 => 'en', text2 => 'niels in english' );    
  scs_text.destroy( text_id => tid7 );
  scs_test.testBool(  'destroy', 3, scs_text.exists_p( tid7, 'da' ) = 'f' 
				AND scs_text.exists_p( tid7, 'en' ) = 'f');
  -- illegal text_id  
  scs_test.testUnit( 'destroy', 4, '
    begin scs_text.destroy( text_id => -9999 ); end; '); 
  -- illegal text_id and legal language 
  scs_test.testUnit( 'destroy', 5, '
    begin scs_text.destroy( text_id => -9999, language => ''da'' ); end; '); 
  -- illegal text_id and illegal language 
  scs_test.testUnit( 'destroy', 6, '
    begin scs_text.destroy( text_id => -9999, language => ''ss'' ); end; '); 
  -- legal text_id and illegal language 
  tid7 := scs_text.updateText( language => 'da', text => 'niels på dansk',
			       language2 => 'en', text2 => 'niels in english' );
  scs_test.testUnit( 'destroy', 7, '
    begin scs_text.destroy( text_id => ' || to_char(tid7) 
      || ', language => ''ss'' ); end; ');
 
  scs_test.printl( 'cleaning up texts: ' );
  scs_text.destroy( tid1 );
  scs_text.destroy( tid2 );
  scs_text.destroy( tid3 );
  scs_text.destroy( tid4 );
  scs_text.destroy( tid5 );
  scs_text.destroy( tid6 );
  scs_text.destroy( tid7 );
  select count(*) into counter1_a from scs_texts;
  select count(*) into counter2_a from scs_text_lang;
  scs_test.testBool( 'garbage test', 0, counter1_b = counter1_a
				    AND counter2_b = counter2_a );
end;
/
