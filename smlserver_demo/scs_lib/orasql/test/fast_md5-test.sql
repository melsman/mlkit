/* ======================================================================
   test suites for the fast_md5 package

   $Id$

   History: 
   040403 Kennie Nybo Pontoppidan <kennie@it-c.dk> created test suite
====================================================================== */

set serveroutput on


begin
  scs_test.printl( '------------------------------' );
  scs_test.printl( 'testing ''fast_md5'' package' );
  scs_test.printl( '------------------------------' );

  scs_test.printl( 'testing function ''md5_string'':' );
  scs_test.testBool( 'md5_string', 1,  fast_md5.md5_string('') = 'd41d8cd98f00b204e9800998ecf8427e');
  scs_test.testBool( 'md5_string', 2,  fast_md5.md5_string(' ' ) = '7215ee9c7d9dc229d2921a40e899ec5f' );
  scs_test.testBool( 'md5_string', 3,  fast_md5.md5_string('a' ) = '0cc175b9c0f1b6a831c399e269772661');
  scs_test.testBool( 'md5_string', 4,  fast_md5.md5_string('qwerty') =  'd8578edf8458ce06fbc5bb76a58c5ca4');
  scs_test.testBool( 'md5_string', 5,  fast_md5.md5_string('LKJHGFDSA' ) = '2ba08e9eafd6b1b2bef03c4f42c7df9f');

  scs_test.testBool( 'md5_string', 6,  fast_md5.md5_string('as12ZX!"+09()BBnn' ) = '719d1f955ee2f86c619af222ae8cb89e');
  scs_test.testBool( 'md5_string', 7,  fast_md5.md5_string('10101010101010101092837465' ) = '0dda215d157837097063502f79685fe0' );
  scs_test.testBool( 'md5_string', 8,  fast_md5.md5_string('0dda215d157837097063502f79685fe0' ) = '3ec8420592387c4db93918ae09f7baa0');
  scs_test.testBool( 'md5_string', 9,  fast_md5.md5_string('dddddddddddddddddddddddddddddddddddddddd' ) = '8fb0b7eccb44c7560f36836c28bb124e');

/* 
  scs_test.testBool( 'md5_string', 10, fast_md5.md5_string() = );

  scs_test.testBool( 'md5_string', 11,  fast_md5.md5_string() =  );
  scs_test.testBool( 'md5_string', 12,  fast_md5.md5_string() =  );
  scs_test.testBool( 'md5_string', 13,  fast_md5.md5_string() =  );
  scs_test.testBool( 'md5_string', 14,  fast_md5.md5_string() =  );
  scs_test.testBool( 'md5_string', 15,  fast_md5.md5_string() =   );
*/
end;
/
show errors
