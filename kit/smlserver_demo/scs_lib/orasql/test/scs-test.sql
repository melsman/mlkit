/* ======================================================================
   test suite for scs_user package

   $Id$

   History: 
   221102 Kennie Nybo Pontoppidan created test suite
====================================================================== */

set serveroutput on;

declare
  
begin
  scs_test.printl( '-------------------' );
  scs_test.printl( 'testing scs package' );
  scs_test.printl( '-------------------' );
  scs_test.printl( 'testing function ''invalidate_field'':' );
  scs_test.testBool( 'invalidate_field', 1, 
    scs.invalidate_field( 'kennie', 10, '11' ) = '11-kennie' );
  scs_test.testBool( 'invalidate_field', 2, 
    scs.invalidate_field( 'kenniekennie', 10, '11' ) = '11-kenniek' );
  scs_test.testBool( 'invalidate_field', 3, 
    scs.invalidate_field( null, 10, '11' ) = '11-' );

end;
/
show errors
