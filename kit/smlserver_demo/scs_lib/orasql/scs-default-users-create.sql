-------------------
-- SCS USER DATA --
-------------------

declare 
  uid scs_users.user_id%TYPE;
  role_id integer;
begin
  -- SiteAdm is the role giving sitewide administrator rights
  role_id := scs_role.new(abbreviation => 'SiteAdm',
                          role_description_tid => 
                            scs_text.updateText(language => 'da',
                                                text => 'Site administrator',
                                                language2 => 'en',
                                                text2 => 'Site administrator'));


  -- We hard code user_id 1 as being the site wide administrator
  uid := scs_user.new(user_id        => '1',
                      password       => '999£Phm$',
		      -- huskeregel: 999 engelske pingviner har mange penge
                      salt           => '12345666554321dlksaælfdsa',
                      modifying_user => '1',
                      email          => 'siteadm@it-c.dk',
                      first_names    => 'Site-wide SCS Administrator',
                      last_name      => 'SCS');
  scs_role.add(party_id => uid, role_id => role_id);

  -- We hard code user_id 0 as being the "not logged in" user
  uid := scs_user.new(user_id        => '0',
                      password       => '',
                      salt           => '',
                      modifying_user => '1',
                      email          => '',
                      first_names    => 'Anonymous User',
                      last_name      => 'Anonymous User');

end;
/ 
show errors



