/***************************************************************************
**
**   PL/SQL MD5 Message-Digest Algorithm
**
**   Version 1.0, 10/7/98
**   Debugged May 22, 2000
**   Updated August 26, 2000
**   Licensing Updated April 29, 2002
**
**   Uses Fast 32-bit Unsigned Binary Operations Library
**
**   **************   
**   You must download and start fast_bw.txt to use this package...
**   http://www.gt.ed.net/keith/plsql/fast_bw.txt
**   **************   
**
**   PL/SQL implementation derived from the C implementation in RFC-1321
**   and derived from the RSA Data Security, Inc. MD5 Message-Digest
**   Algorithm.  See RFC-1321 for details.
**
**   Written by Keith Gardner
**   Email: keith.gardner@gtri.gatech.edu
**
**   Debugged by Peter Vincen
**   University of Economics, Bratislava, Slovak Republic
**
**   I removed the Oracle Web Server (OWS) specific code and replaced
**   it with dbms_output code and added a line to the code to set
**   server output on.  This way you can give the code a try to see
**   if you get the correct result of f96b697d7cb7938d525a2f31aaf161d0
**   in sql*plus after you start fast_bw.txt and fast_md5.txt
**
**        execute fast_md5.md5('message digest');
**
**   I hear reports that this algorithm does not work with non-US7-ASCII
**   character sets.
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
**   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  
**   02111-1307  USA
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
**-------------------------------------------------------------------------
**
**   Copyright (C) 1990-2, RSA Data Security, Inc. All rights reserved.
**
**   License to copy and use this software is granted provided that it
**   is identified as the "RSA Data Security, Inc. MD5 Message-Digest
**   Algorithm" in all material mentioning or referencing this software
**   or this function.
**
**   License is also granted to make and use derivative works provided
**   that such works are identified as "derived from the RSA Data
**   Security, Inc. MD5 Message-Digest Algorithm" in all material
**   mentioning or referencing the derived work.
**
**   RSA Data Security, Inc. makes no representations concerning either
**   the merchantability of this software or the suitability of this
**   software for any particular purpose. It is provided "as is"
**   without express or implied warranty of any kind.
**
**   These notices must be retained in any copies of any part of this
**   documentation and/or software.
**
***************************************************************************/

set serveroutput on;

create or replace
package fast_md5 as

   -- digests a string using md5 and returns the result.
   function md5_string (str in varchar2) return varchar2;
/* commented out by Kennie Nybo Pontoppidan, 040403
   procedure md5 (str in varchar2);
   procedure md5_trial;
*/
   PRAGMA RESTRICT_REFERENCES(fast_md5, WNDS);
   PRAGMA RESTRICT_REFERENCES(md5_string, WNDS);

end fast_md5;

/
create or replace package body fast_md5 as

/*************************************************************************** 
**
** DECLARATION
**
***************************************************************************/

   -- generic md5 word type
   md5_word number;

   -- array of md5 words
   type md5_array is table of md5_word%type
      index by binary_integer;

   -- md5 context
   type md5_ctx is record (
      state md5_array, -- 4 rows
      count md5_array, -- 2 rows
      buffer varchar2(64)
   );

   -- constants for md5transform procedure
   S11 md5_word%type := 7;
   S12 md5_word%type := 12;
   S13 md5_word%type := 17;
   S14 md5_word%type := 22;
   S21 md5_word%type := 5;
   S22 md5_word%type := 9;
   S23 md5_word%type := 14;
   S24 md5_word%type := 20;
   S31 md5_word%type := 4;
   S32 md5_word%type := 11;
   S33 md5_word%type := 16;
   S34 md5_word%type := 23;
   S41 md5_word%type := 6;
   S42 md5_word%type := 10;
   S43 md5_word%type := 15;
   S44 md5_word%type := 21;

   -- message padding with length congruent to 448, modulo 512.
   PADDING varchar2(64) := 
      chr(128) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0) || 
      chr(  0) || chr(0) || chr(0) || chr(0);
 
   /*
   ** 1 of 4 basic MD5 32-bit word functions
   */

   function F (
      x in md5_word%type,
      y in md5_word%type,
      z in md5_word%type)
      return md5_word%type is
   begin
      return (fbw.op(
          fbw.op(x,'and',y),'or',
          fbw.op(fbw.comp(x),'and',z)));
   end F;

   /*
   ** 2 of 4 basic MD5 32-bit word functions
   */

   function G (
      x in md5_word%type,
      y in md5_word%type,
      z in md5_word%type)
      return md5_word%type is
   begin
      return fbw.op(fbw.op(z,'and',x),'or',
         fbw.op(fbw.comp(z),'and',y));
   end G;

   /*
   ** 3 of 4 basic MD5 32-bit word functions
   */

   function H (
      x in md5_word%type,
      y in md5_word%type,
      z in md5_word%type)
      return md5_word%type is
   begin
      return (fbw.op(fbw.op(x,'xor',y),'xor',z));
   end H;

   /*
   ** 4 of 4 basic MD5 32-bit word functions
   */

   function I (
      x in md5_word%type,
      y in md5_word%type,
      z in md5_word%type)
      return md5_word%type is
   begin
      return (fbw.op(y,'xor',fbw.op(x,'or',fbw.comp(z))));
   end I;

   /*
   ** Transformation for round 1.
   */

   function FF (
      a in md5_word%type,
      b in md5_word%type,
      c in md5_word%type,
      d in md5_word%type,
      x in md5_word%type,
      s in md5_word%type,
      ac in md5_word%type)
      return md5_word%type is
   begin
      return fbw.lrot(a + F(b,c,d) + x + ac,s) + b;
   end FF;

   /*
   ** Transformation for round 2.
   */

   function GG (
      a in md5_word%type,
      b in md5_word%type,
      c in md5_word%type,
      d in md5_word%type,
      x in md5_word%type,
      s in md5_word%type,
      ac in md5_word%type)
      return md5_word%type is
   begin
      return fbw.lrot(a + G(b,c,d) + x + ac,s) + b;
   end GG;

   /*
   ** Transformation for round 3.
   */

   function HH (
      a in md5_word%type,
      b in md5_word%type,
      c in md5_word%type,
      d in md5_word%type,
      x in md5_word%type,
      s in md5_word%type,
      ac in md5_word%type)
      return md5_word%type is
   begin
      return fbw.lrot(a + H(b,c,d) + x + ac,s) + b;
   end HH;

   /*
   ** Transformation for round 4.
   */

   function II (
      a in md5_word%type,
      b in md5_word%type,
      c in md5_word%type,
      d in md5_word%type,
      x in md5_word%type,
      s in md5_word%type,
      ac in md5_word%type)
      return md5_word%type is
   begin
      return fbw.lrot(a + I(b,c,d) + x + ac,s) + b;
   end II;

   procedure md5_init (
      context in out md5_ctx);
   procedure md5_update (
      context in out md5_ctx,
      input in varchar2,
      input_len in md5_word%type);
   procedure md5_final (
      digest in out varchar2,
      context in out md5_ctx);
   procedure md5_transform (
      state in out md5_array,
      block in varchar2);
   procedure encode (
      output in out varchar2,
      input in md5_array,
      len in binary_integer);
   procedure decode (
      output in out md5_array,
      input in varchar2,
      len in binary_integer);
   function md5_memcpy (
      output in varchar2,
      output_start in md5_word%type,
      input in varchar2,
      len in md5_word%type)
   return varchar2;

/*************************************************************************** 
**
** PROCEDURES AND NON-DECLARATIVE FUNCTIONS
**
***************************************************************************/

   /*
   ** Digests a string and prints the result for Oracle Web Server
   */

   procedure md5 (
      str in varchar2)
   is

      context md5_ctx;
      digest varchar2(16);
      len binary_integer;

      time_before binary_integer;
      time_after binary_integer;

   begin

/*
      owa_util.mime_header ('text/plain', TRUE);
      htp.print ('fast_md5.md5: str=' || str);
*/
      dbms_output.put_line ('fast_md5.md5: str=' || str);

      len := length(str);
      if (len is null) then
          len := 0;
      end if;

      time_before := dbms_utility.get_time;

      md5_init(context);
      md5_update(context, str, len);
      md5_final(digest, context);

      time_after := dbms_utility.get_time;

/*
      htp.print ('fast_md5.md5: digest=' || fbw.hexdump(digest));
      htp.print ('fast_md5.md5: time=' || ((time_after - time_before)/100) || ' seconds');
*/
      dbms_output.put_line ('fast_md5.md5: digest=' || fbw.hexdump(digest));
      dbms_output.put_line ('fast_md5.md5: time=' || ((time_after - time_before)/100) || ' seconds');

   exception
      when others then
/*
         htp.print (SQLERRM);
*/
         dbms_output.put_line (SQLERRM);

   end md5;

   /*     
   ** Performs a timed trial and prints the result for Oracle Web Server
   */
      
   procedure md5_trial is
      
      context md5_ctx;
      digest varchar2(16);
      tblock varchar2(100);
      
      time_before binary_integer;
      time_after binary_integer;

   begin

/*
      owa_util.mime_header ('text/plain', TRUE);
      htp.print ('MD-5 time trial. Digesting 10 100-byte blocks ...');
*/
      dbms_output.put_line ('MD-5 time trial. Digesting 10 100-byte blocks ...');

      tblock := ''; 
      for i in 1..100 loop
          tblock := tblock || chr(fbw.op(i,'and',255));
      end loop;
   
      time_before := dbms_utility.get_time;
      
      md5_init(context);  
      for i in 1..10 loop
          md5_update(context, tblock, 100);
      end loop;
      md5_final(digest, context);
     
      time_after := dbms_utility.get_time;

/*      
      htp.print ('digest=' || fbw.hexdump(digest));
      htp.print ('time=' || ((time_after - time_before)/100) || ' seconds');
      htp.print ('speed=' || (10 * 100 * 100/(time_after - time_before)) || ' bytes/second');
*/
      dbms_output.put_line ('digest=' || fbw.hexdump(digest));
      dbms_output.put_line ('time=' || ((time_after - time_before)/100) || ' seconds');
      dbms_output.put_line ('speed=' || (10 * 100 * 100/(time_after - time_before)) || ' bytes/second');

   exception
      when others then
/*
         htp.print (SQLERRM);
*/
         dbms_output.put_line (SQLERRM);
      
   end md5_trial;

   /* 
   ** Digests a string using md5 and returns the result.
   */

   function md5_string (
      str in varchar2)
      return varchar2 is

      context md5_ctx;
      digest varchar2(16);
      len md5_word%type;

   begin

      len := length(str);
      if (len is null) then
          len := 0;
      end if;

      md5_init(context);
      md5_update(context, str, len);
      md5_final(digest, context);

      return (fbw.hexdump(digest));

   end md5_string;

   /* 
   ** MD5 initialization. Begins an MD5 operation, writing a new context.
   */

   procedure md5_init (context in out md5_ctx) is

   begin

      -- clear the count
      context.count(0) := 0;
      context.count(1) := 0;    

      -- load magic initialization constants
      context.state(0) := 1732584193;
      context.state(1) := 4023233417;
      context.state(2) := 2562383102;
      context.state(3) := 271733878;

   end md5_init;

   /*
   ** MD5 block update operation. Continues an MD5 message-digest
   ** operation, processing another message block, and updating the
   ** context.
   */

   procedure md5_update (
      context in out md5_ctx,
      input in varchar2,
      input_len in md5_word%type) is

      i md5_word%type;
      idx md5_word%type;
      part_len md5_word%type;
      tmp_string varchar2(64);

   begin

      -- compute number of bytes mod 64
      idx := fbw.op(fbw.rshift(context.count(0),3),'and',63);

      -- update number of bits
      context.count(0) := context.count(0) + fbw.lshift(input_len,3);
      if (context.count(0) < fbw.lshift(input_len,3)) then
         context.count(1) := context.count(1) + 1;
      end if;
      context.count(1) := context.count(1) + fbw.rshift(input_len,29);

      part_len := 64 - idx;

      -- transform as many times as possible
      if (input_len >= part_len) then
         context.buffer := md5_memcpy (context.buffer, idx, input, part_len);
         md5_transform(context.state, context.buffer);

         i := part_len;
         while (i + 63 < input_len) loop
             md5_transform(context.state, substr(input, i + 1));
             i := i + 64;
         end loop;

         idx := 0;
      else
         i := 0;
      end if;

      -- buffer remaining input
      context.buffer := md5_memcpy (context.buffer, idx,
         substr(input, i + 1), input_len - i);

   end md5_update;

   /*
   ** MD5 finalization. Ends an MD5 message-digest operation, writing the
   ** the message digest and zeroizing the context.
   */

   procedure md5_final (
      digest in out varchar2,
      context in out md5_ctx) is

      bits varchar2(8);
      idx md5_word%type;
      pad_len md5_word%type;

   begin

      -- save number of bits
      encode(bits, context.count, 8);

      -- pad out to 56 mod 64
      idx := fbw.op(fbw.rshift(context.count(0),3),'and',63);
      if (idx < 56) then
         pad_len := 56 - idx;
      else
         pad_len := 120 - idx;
      end if;
      md5_update(context, PADDING, pad_len);

      -- append length (before padding)
      md5_update(context, bits, 8);

      -- store state in digest
      encode(digest, context.state, 16);

      -- zeroize sensitive information
      md5_init(context);
      context.buffer := '';

   end md5_final;

   /*
   ** MD5 basic transformation. Transforms state based on block.
   */

   procedure md5_transform (
      state in out md5_array,
      block in varchar2) is

      a md5_word%type;
      b md5_word%type;
      c md5_word%type;
      d md5_word%type;
      x md5_array;

   begin

      a := state(0);
      b := state(1);
      c := state(2);
      d := state(3);

      for i in 0..15 loop
         x(i) := 0;
      end loop;
      
      decode(x, block, 64);

      -- round 1
      a := FF (a, b, c, d, x( 0), S11, 3614090360); -- 1
      d := FF (d, a, b, c, x( 1), S12, 3905402710); -- 2
      c := FF (c, d, a, b, x( 2), S13, 606105819);  -- 3
      b := FF (b, c, d, a, x( 3), S14, 3250441966); -- 4
      a := FF (a, b, c, d, x( 4), S11, 4118548399); -- 5
      d := FF (d, a, b, c, x( 5), S12, 1200080426); -- 6
      c := FF (c, d, a, b, x( 6), S13, 2821735955); -- 7
      b := FF (b, c, d, a, x( 7), S14, 4249261313); -- 8
      a := FF (a, b, c, d, x( 8), S11, 1770035416); -- 9
      d := FF (d, a, b, c, x( 9), S12, 2336552879); -- 10
      c := FF (c, d, a, b, x(10), S13, 4294925233); -- 11
      b := FF (b, c, d, a, x(11), S14, 2304563134); -- 12
      a := FF (a, b, c, d, x(12), S11, 1804603682); -- 13
      d := FF (d, a, b, c, x(13), S12, 4254626195); -- 14
      c := FF (c, d, a, b, x(14), S13, 2792965006); -- 15
      b := FF (b, c, d, a, x(15), S14, 1236535329); -- 16

      -- round 2
      a := GG (a, b, c, d, x( 1), S21, 4129170786); -- 17
      d := GG (d, a, b, c, x( 6), S22, 3225465664); -- 18
      c := GG (c, d, a, b, x(11), S23, 643717713);  -- 19
      b := GG (b, c, d, a, x( 0), S24, 3921069994); -- 20
      a := GG (a, b, c, d, x( 5), S21, 3593408605); -- 21
      d := GG (d, a, b, c, x(10), S22, 38016083);   -- 22
      c := GG (c, d, a, b, x(15), S23, 3634488961); -- 23
      b := GG (b, c, d, a, x( 4), S24, 3889429448); -- 24
      a := GG (a, b, c, d, x( 9), S21, 568446438);  -- 25
      d := GG (d, a, b, c, x(14), S22, 3275163606); -- 26
      c := GG (c, d, a, b, x( 3), S23, 4107603335); -- 27
      b := GG (b, c, d, a, x( 8), S24, 1163531501); -- 28
      a := GG (a, b, c, d, x(13), S21, 2850285829); -- 29
      d := GG (d, a, b, c, x( 2), S22, 4243563512); -- 30
      c := GG (c, d, a, b, x( 7), S23, 1735328473); -- 31
      b := GG (b, c, d, a, x(12), S24, 2368359562); -- 32

      -- round 3
      a := HH (a, b, c, d, x( 5), S31, 4294588738); -- 33
      d := HH (d, a, b, c, x( 8), S32, 2272392833); -- 34
      c := HH (c, d, a, b, x(11), S33, 1839030562); -- 35
      b := HH (b, c, d, a, x(14), S34, 4259657740); -- 36
      a := HH (a, b, c, d, x( 1), S31, 2763975236); -- 37
      d := HH (d, a, b, c, x( 4), S32, 1272893353); -- 38
      c := HH (c, d, a, b, x( 7), S33, 4139469664); -- 39
      b := HH (b, c, d, a, x(10), S34, 3200236656); -- 40
      a := HH (a, b, c, d, x(13), S31, 681279174);  -- 41
      d := HH (d, a, b, c, x( 0), S32, 3936430074); -- 42
      c := HH (c, d, a, b, x( 3), S33, 3572445317); -- 43
      b := HH (b, c, d, a, x( 6), S34, 76029189);   -- 44
      a := HH (a, b, c, d, x( 9), S31, 3654602809); -- 45
      d := HH (d, a, b, c, x(12), S32, 3873151461); -- 46
      c := HH (c, d, a, b, x(15), S33, 530742520);  -- 47
      b := HH (b, c, d, a, x( 2), S34, 3299628645); -- 48

      -- round 4
      a := II (a, b, c, d, x( 0), S41, 4096336452); -- 49
      d := II (d, a, b, c, x( 7), S42, 1126891415); -- 50
      c := II (c, d, a, b, x(14), S43, 2878612391); -- 51
      b := II (b, c, d, a, x( 5), S44, 4237533241); -- 52
      a := II (a, b, c, d, x(12), S41, 1700485571); -- 53
      d := II (d, a, b, c, x( 3), S42, 2399980690); -- 54
      c := II (c, d, a, b, x(10), S43, 4293915773); -- 55
      b := II (b, c, d, a, x( 1), S44, 2240044497); -- 56
      a := II (a, b, c, d, x( 8), S41, 1873313359); -- 57
      d := II (d, a, b, c, x(15), S42, 4264355552); -- 58
      c := II (c, d, a, b, x( 6), S43, 2734768916); -- 59
      b := II (b, c, d, a, x(13), S44, 1309151649); -- 60
      a := II (a, b, c, d, x( 4), S41, 4149444226); -- 61
      d := II (d, a, b, c, x(11), S42, 3174756917); -- 62
      c := II (c, d, a, b, x( 2), S43, 718787259);  -- 63
      b := II (b, c, d, a, x( 9), S44, 3951481745); -- 64

      state(0) := state(0) + a;
      state(1) := state(1) + b;
      state(2) := state(2) + c;
      state(3) := state(3) + d;

      -- zeroize sensitive information
      for i in 0..15 loop
         x(i) := 0;
      end loop;

   end md5_transform;

   /*
   ** Encodes input (UINT4) into output (unsigned char). Assumes len is
   ** a multiple of 4.
   */

   procedure encode (
      output in out varchar2,
      input in md5_array,
      len in binary_integer) is

      i binary_integer;
      j binary_integer;

   begin

      i := 0;
      j := 0;
      output := '';
      while (j < len) loop
         output := output || chr(fbw.op(input(i),'and',255));
         output := output || chr(fbw.op(fbw.rshift(input(i),8),'and',255));
         output := output || chr(fbw.op(fbw.rshift(input(i),16),'and',255));
         output := output || chr(fbw.op(fbw.rshift(input(i),24),'and',255));
         i := i + 1;
         j := j + 4;
      end loop;

   end encode;

   /*
   ** Decodes input (unsigned char) into output (UINT4). Assumes len is
   ** a multiple of 4.
   */

   procedure decode (
      output in out md5_array,
      input in varchar2,
      len in binary_integer) is

      i binary_integer;
      j binary_integer;

   begin

      i := 0;
      j := 0;
      while (j < len) loop
         output(i) := fbw.op(fbw.op(fbw.op(ascii(substr(input,j+1,1)),'or',
            fbw.lshift(ascii(substr(input,j+2,1)),8)),'or',
            fbw.lshift(ascii(substr(input,j+3,1)),16)),'or',
            fbw.lshift(ascii(substr(input,j+4,1)),24));
         i := i + 1;
         j := j + 4;
      end loop;

   end decode;

   /*
   ** MD5 memcpy is a substring manipulator in PL/SQL.
   */

   function md5_memcpy (
      output in varchar2,
      output_start in md5_word%type,
      input in varchar2,
      len in md5_word%type)
      return varchar2 is

      str varchar2(64);
      part1 varchar2(64);
      part2 varchar2(64);
      part3 varchar2(64);

   begin

      part1 := substr(output, 1, output_start);
      part2 := substr(input, 1, len);
      if (output_start + len + 1 < length(output)) then
         part3 := substr(output, output_start + len + 1);
      end if;

      str := '';
      if (part1 is not null) then
         str := part1;
      end if;
      if (part2 is not null) then
         str := str || part2;
      end if;
      if (part3 is not null) then
         str := str || part3;
      end if;

      return str;

   end md5_memcpy;   

end fast_md5;

/
show errors;
