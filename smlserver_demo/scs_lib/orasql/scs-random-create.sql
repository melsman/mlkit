/*
------------------------------------------------------------------------------
Filename:  random.txt
Purpose:   Random number/ string generator package
Author:    Unknown
Original:  http://www.orafaq.org/scripts/sql/random.txt
Taken from: http://orafaq.cs.rmit.edu.au/scripts/plsql/random.txt
Edits:
 19990908 Phil Rand <prand@spu.edu> Added functions rand_string().
 20021015 Niels Hallenberg <nh@it.edu> Edited names and added random password function
------------------------------------------------------------------------------
*/

create or replace package scs_random
is
   procedure srand (new_seed in number);
   procedure get_rand (r OUT number);
   procedure get_rand_max(r OUT number, n IN number);
   function  rand return number;
   function  rand_max(n IN number) return number;
   function  rand_string(ssiz IN number) return varchar2;

   /* http://download-west.oracle.com/otndoc/oracle9i/901_doc/server.901/a90125/statements_82.htm: */
   /*   WNDS means does not modify database tables */
   /*   RNPS means does not query database tables */
   pragma restrict_references(rand, WNDS);
   pragma restrict_references(rand_max, WNDS);
   pragma restrict_references(scs_random, WNDS, RNPS);
   pragma restrict_references(rand_string, WNDS);
end scs_random;
/
show errors

create or replace package body scs_random
is
   multiplier   constant number := 22695477;
   increment    constant number := 1;
   "2^32"       constant number := 2 ** 32;
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
     Seed := mod(multiplier * Seed + increment, "2^32");
     return bitand(Seed/"2^16", "0x7fff");
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

