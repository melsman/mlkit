create or replace package scs_math
is
   function  min(x IN number, y IN number) return number;
   function  max(x IN number, y IN number) return number;
   /* http://download-west.oracle.com/otndoc/oracle9i/901_doc/server.901/a90125/statements_82.htm: */
   /*   WNDS means does not modify database tables */
   /*   RNPS means does not query database tables */
   pragma restrict_references(min, WNDS);
   pragma restrict_references(max, WNDS);
end scs_math;
/
show errors

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
