/* 
Patch 001 for scs-file-storage

Date:   2003-11-18
Author: Niels Hallenberg
Purpose: 
* Add pre-computation of physical path on disk to folders, field: path_on_disk.

  This optimization only works because we can't move folders around,
  that is, a folder once given a physical location on disk never
  changes.

  If folder f is

    foo/bar/f

  then label i f and path_on_disk is foo/bar/

* Update physical paths on existing folders in the database

* A few functions has also been added to scs-file-storage-packages-create.

*/

alter table scs_fs_folders
  add path_on_disk varchar(1000);

/* Necessary for the code below */
@scs-file-storage-packages-create

/* Update folders already in the database */
declare
  r scs_fs_folders%ROWTYPE;
  path varchar(1000);
begin
  for r in (select * from scs_fs_folders)
  loop
    path := scs_file_storage.getPath(r.folder_id);
    update scs_fs_folders
       set path_on_disk = path
     where folder_id = r.folder_id;
  end loop;
end;
/
show errors

commit;


