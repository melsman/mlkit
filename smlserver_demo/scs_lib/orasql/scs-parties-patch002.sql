/* 
Patch 002 for scs-parties

Date:   2003-11-18
Author: Kennie Nybo Pontoppidan
Purpose: 
  * Add a function index on scs_parties.email 
*/

create index scs_parties_lower_email_idx on scs_parties( lower(email) );
