-- $Id$

/* ========================================
   package scs_math
======================================== */
create or replace package scs_math
is
   /* ------------
      function min
      ------------
      returns the minimum of two numbers x and y
   */
   function min(x IN number, y IN number) return number;

   /* ------------
      function max
      ------------
      returns the maximum of two numbers x and y
   */
   function max(x IN number, y IN number) return number;

   /* 
      WNDS means does not modify database tables
      RNPS means does not query database tables
      see http://download-west.oracle.com/otndoc/oracle9i/901_doc/server.901/a90125/statements_82.htm 
   */
   pragma restrict_references(min, WNDS);
   pragma restrict_references(max, WNDS);
end scs_math;
/
show errors


/*======================================================================
  package bodies begin here
====================================================================== */
create or replace package body scs_math
is
   function min(
     x IN number, 
     y IN number
   ) return number 
   is
   begin
     if x <= y then
       return x;
     else
       return y;
     end if;
   end min;

   function max(
     x IN number, 
     y IN number
   ) return number 
   is
   begin
     if x <= y then
       return y;
     else
       return x;
     end if;
   end max;
end scs_math;
/
show errors
