/* 
Patch 003 for scs-file-storage

Date:   2005-05-10, 
Author: Kennie Nybo Pontoppidan
Purpose: 

  * added icons for word, excel and powerpoint mime types

*/

update scs_fs_mime_types 
   set file_icon = '/ucs/images/icon_ppt.gif'
 where file_extension = 'ppt';

update scs_fs_mime_types 
   set file_icon = '/ucs/images/icon_Word.gif'
 where file_extension = 'doc';

update scs_fs_mime_types 
   set file_icon = '/ucs/images/icon_Excel.gif'
 where file_extension = 'xls';
