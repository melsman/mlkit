-- $Id$

/* ======================================================================
   package scs_math

   math library

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> Added comments
   171002 Niels Hallenberg <nh@it.edu> Created package
====================================================================== */
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

   /* pragma directives:
        WNDS means does not modify database tables
        RNPS means does not query database tables
      see http://download-west.oracle.com/otndoc/oracle9i/901_doc/server.901/a90125/statements_82.htm 
   */
   pragma restrict_references(min, WNDS);
   pragma restrict_references(max, WNDS);
end scs_math;
/
show errors


/* ======================================================================
   package scs_random

   Random number/ string generator package

   NB! (knp): 

   Author:    Unknown
   Original:  http://www.orafaq.org/scripts/sql/random.txt
   Taken from: http://orafaq.cs.rmit.edu.au/scripts/plsql/random.txt

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> Added comments and corrected
	  the random generator to be able to create more than 200 unique values
   151002 Niels Hallenberg <nh@it.edu> Edited names
   080999 Phil Rand <prand@spu.edu> Added functions rand_string().
====================================================================== */

create or replace package scs_random
is
  /* ---------------
     procedure srand
     ---------------
     resets the random generator seed to new_seed    
  */
  procedure srand (new_seed in number);

  /* ------------------
     procedure get_rand
     ------------------
     returns a random number
  */
  procedure get_rand (r OUT number);

  /* ----------------------
     procedure get_rand_max
     ----------------------
     returns a random number below n
  */
  procedure get_rand_max(r OUT number, n IN number);

  /* ------------------
     function rand
     ------------------
     returns a random number
     (same as get_rand, except this is a function, not a procedure)
  */  
  function rand return number;

  /* ------------------
     function rand_max
     ------------------
     returns a random number below n
     (same as get_rand_max, except this is a function, not a procedure)
  */  
  function rand_max(n IN number) return number;

  /* --------------------
     function rand_string
     --------------------
     returns a random string of size ssiz
     (same as get_rand, except this is a function, not a procedure)
  */  
  function rand_string(ssiz IN number) return varchar2;

  /* pragma directives:
       WNDS means does not modify database tables
       RNPS means does not query database tables
     see http://download-west.oracle.com/otndoc/oracle9i/901_doc/server.901/a90125/statements_82.htm
  */
  pragma restrict_references(rand, WNDS);
  pragma restrict_references(rand_max, WNDS);
  pragma restrict_references(scs_random, WNDS, RNPS);
  pragma restrict_references(rand_string, WNDS);
end scs_random;
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


create or replace package body scs_random
is
   multiplier   constant number := 16807;
   increment    constant number := 1;
   "2^32-1"     constant number := (2 ** 32)-1;
   "2^16"       constant number := 2 ** 16;
   "0x7fff"     constant number := 32767;
   Seed         number          := 1;

   function rand_string(
     ssiz IN number
   ) return varchar2 
   is
     i      number;
     m      number;
     c      char;
     result varchar2(2000) := '';
   begin
     m := scs_math.min(ssiz,2000);
     for i in 1..m loop
       c := substr('abcdefghijklmnopqrstuvwxyz0123456789',rand_max(36),1);
       result := result || c;
     end loop;
     return result;
   end rand_string;

   procedure srand(
     new_seed in number
   ) 
   is
   begin
     Seed := new_seed;
   end srand;

   function rand 
   return number 
   is
   begin
     Seed := mod(multiplier * Seed + increment, "2^32-1");
     return Seed;
     -- return bitand(Seed/"2^16", "0x7fff");
   end rand;

   procedure get_rand(
     r OUT number
   ) 
   is
   begin
     r := rand;
   end get_rand;

   function rand_max(
     n IN number
   ) return number 
   is
   begin
     return mod(rand, n) + 1;
   end rand_max;

   procedure get_rand_max(
     r OUT number, 
     n IN number
   ) 
   is
   begin
     r := rand_max(n);
   end get_rand_max;

begin
   select userenv('SESSIONID')
   into   Seed
   from   dual;
end scs_random;
/
show errors
