create or replace package fbw as

/**************************************************************************
**
**   Fast 32-bit Unsigned Binary Operations Library
**
**   Version 1.0, 6/11/98
**   Licensing updated 4/29/02
** 
**   Based on Kevin Taufner's version 1.0, 7/20/97, Util package.
** 
**   Written by Keith Gardner
**   Email: keith.gardner@gtri.gatech.edu
**   
**   Please send comments, corrections, and improvements.
**
**-------------------------------------------------------------------------
**
**   Copyright (C) 1998, 2002 Wallace Keith Gardner.
**   All rights reserved.
**
**   This file is free software; you can redistribute it and/or
**   modify it under the terms of the GNU Lesser General Public
**   License as published by the Free Software Foundation; either
**   version 2.1 of the License, or (at your option) any later version.
**
**   This library is distributed in the hope that it will be useful,
**   but WITHOUT ANY WARRANTY; without even the implied warranty of
**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**   Lesser General Public License for more details.
**
**   You should have received a copy of the GNU Lesser General Public
**   License along with this library; if not, write to the Free Software
**   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
**
**   You must download, read, agree, and retain a copy of the GNU Lesser
**   General Public License with this software. It can be found at:
**
**   http://www.gt.ed.net/keith/lesser.txt
**
**   You must download, read, agree, and retain a copy of the Terms
**   of Use and Disclaimer with this software. It can be found at:
**
**   http://www.gt.ed.net/keith/terms.txt
**
***************************************************************************/

   function op (
      value1 in number, 
      operation in varchar2,
      value2 in number)
      return number;

   function comp (
      value in number)
      return number;

   function lshift (
      value in number, 
      times in integer := 1)
      return number;

   function rshift (
      value in number, 
      times in integer := 1)
      return number;

   function lrot (
      value in number, 
      times in integer := 1)
      return number;

   function rrot (
      value in number, 
      times in integer := 1)
      return number;

   function reword (
      value in number)
      return number;

   function hexdump (
      value in varchar2)
      return varchar2;

    PRAGMA RESTRICT_REFERENCES(fbw, WNDS);
    PRAGMA RESTRICT_REFERENCES(op, WNDS);
    PRAGMA RESTRICT_REFERENCES(comp, WNDS);
    PRAGMA RESTRICT_REFERENCES(lshift, WNDS);
    PRAGMA RESTRICT_REFERENCES(rshift, WNDS);
    PRAGMA RESTRICT_REFERENCES(lrot, WNDS);
    PRAGMA RESTRICT_REFERENCES(rrot, WNDS);
    PRAGMA RESTRICT_REFERENCES(reword, WNDS);
    PRAGMA RESTRICT_REFERENCES(hexdump, WNDS);

end fbw;
/

create or replace package body fbw as

/**********************************************************************
**
** FUNCTION op
**
**********************************************************************/

   function op (
      value1 in number, 
      operation in varchar2,
      value2 in number)
      return number
   is

      working1 integer;
      working2 integer;
      result1 number;
      result2 number;
      result number := 0;

   begin

      -- truncate high bits
      working1 := fbw.reword(value1);
      working2 := fbw.reword(value2);

      -- XOR operation
      if (operation = 'xor') then
         for i in reverse 0..32 loop
            result1 := working1 - (2**(i));
            result2 := working2 - (2**(i));
            if (result1 >= 0 and result2 < 0) then -- 1 and 0
               working1 := result1;
               result := result + (2**(i));
            elsif (result1 < 0 and result2 >= 0) then -- 0 and 1
               working2 := result2;
               result := result + (2**(i));
            elsif (result1 >= 0 and result2 >= 0) then -- 1 and 1
               working1 := result1;
               working2 := result2;
            end if;
         end loop;

      -- OR operation
      elsif (operation = 'or') then
         for i in reverse 0..32 loop
            result1 := working1 - (2**(i));
            result2 := working2 - (2**(i));
            if (result1 >= 0 and result2 < 0) then -- 1 and 0
               working1 := result1;
               result := result + (2**(i));
            elsif (result1 < 0 and result2 >= 0) then -- 0 and 1
               working2 := result2;
               result := result + (2**(i));
            elsif (result1 >= 0 and result2 >= 0) then -- 1 and 1
               working1 := result1;
               working2 := result2;
               result := result + (2**(i));
            end if;
         end loop;
          
      -- AND operation
      else
         for i in reverse 0..32 loop
            result1 := working1 - (2**(i));
            result2 := working2 - (2**(i));
            if (result1 >= 0 and result2 < 0) then -- 1 and 0
               working1 := result1;
            elsif (result1 < 0 and result2 >= 0) then -- 0 and 1
               working2 := result2;
            elsif (result1 >= 0 and result2 >= 0) then -- 1 and 1
               working1 := result1;
               working2 := result2;
               result := result + (2**(i));
            end if;
         end loop;

      end if;

      -- return the result
      return result;

   end op;

/**********************************************************************
**
** FUNCTION comp
**
**********************************************************************/

   function comp (
      value in number)
      return number
   is

      working1 integer;
      working2 integer;
      result1 number;
      result2 number;
      result number := 0;

   begin

      -- truncate high bits and setup 32-bit mask
      working1 := fbw.reword(value);
      working2 := 4294967295;

      -- calculate the complement by taking the Xor
      for i in reverse 0..32 loop
         result1 := working1 - (2**(i));
         result2 := working2 - (2**(i));
         if (result1 >= 0 and result2 < 0) then -- 1 and 0
            working1 := result1;
            result := result + 2**(i);
         elsif (result1 < 0 and result2 >= 0) then -- 0 and 1
            working2 := result2;
            result := result + 2**(i);
         elsif (result1 >= 0 and result2 >= 0) then -- 1 and 1
            working1 := result1;
            working2 := result2;
         end if;
      end loop;

      -- return the result
      return result;

   end comp;

/**********************************************************************
**
** FUNCTION lshift
**
**********************************************************************/

   function lshift (
      value in number, 
      times in integer := 1)
      return number
   is

   begin

      return fbw.reword(value * (2**(times)));

   end lshift;

/**********************************************************************
**
** FUNCTION rshift
**
**********************************************************************/
   
   function rshift (
      value in number, 
      times in integer := 1)
      return number
   is

   begin

      return trunc(fbw.reword(value) / (2**(times)));

   end rshift;

/**********************************************************************
**
** FUNCTION lrot
**
**********************************************************************/

   function lrot (
      value in number,
      times in integer := 1)
      return number
   is

      result number := 0;

   begin

      -- truncate high bits
      result := fbw.reword(value);

      -- rotate left
      return fbw.op(
         fbw.lshift(result, times), 'or',
         fbw.rshift(result, 32 - times));

   end lrot;

/**********************************************************************
**
** FUNCTION rrot
**
**********************************************************************/

   function rrot (
      value in number,
      times in integer := 1)
      return number
   is

      result number := 0;

   begin

      -- truncate high bits
      result := fbw.reword(value);

      -- rotate right
      return fbw.op(
         fbw.lshift(result, 32 - times), 'or',
         fbw.rshift(result, times));

   end rrot;

/**********************************************************************
**
** FUNCTION reword
**
**********************************************************************/

   function reword (
      value in number)
      return number
   is

      result number := 0;

   begin

      -- truncate high bits greater than the preferred word size
      if (value > 4294967295) then
          result := mod(value, 4294967296);
      else
          result := value;
      end if;

      -- return the result
      return result;

   end reword;

/**********************************************************************
**
** FUNCTION hexdump
**
**********************************************************************/

   function hexdump (
      value in varchar2)
      return varchar2
   is

      len integer;
      dec_value integer;
      dec_nibble1 integer;
      dec_nibble2 integer;
      result varchar2(2000);
      hex_digits varchar2(16) := '0123456789abcdef';
      tmpf number;

   begin

      -- determine length of string
      len := length (value);
      if (len is null) then
          len := 0;
      end if;

      -- find the hex representation of each character
      result := '';
      for i in 1..len loop
         dec_value := ascii(substr(value, i, 1));
	 tmpf := dec_value;
	 dec_nibble1 := trunc( tmpf / 16);
	 dec_nibble2 := mod(dec_value, 16);
         result := result || substr(hex_digits, dec_nibble1 + 1, 1);
         result := result || substr(hex_digits, dec_nibble2 + 1, 1);
      end loop;
      
      -- return the result as a string of hex
      return result;

   end hexdump;

end fbw;
/

show errors;
