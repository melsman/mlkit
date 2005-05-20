/* 
Patch 002 for scs-file-storage

Date:   2005-04-26, 
Author: Niels Hallenberg
Purpose: 

  * Re-defining constrains on fs_folders:

*/

alter table scs_fs_folders
 drop constraint scs_fs_folders_label_un;
alter table scs_fs_folders
  add constraint scs_fs_folders_label_un unique (instance_id,parent_id,label);

alter table scs_fs_folders
 drop constraint scs_fs_folders_folder_un;
alter table scs_fs_folders
  add constraint scs_fs_folders_folder_un unique (instance_id,parent_id,foldername);

