val _ =
  let
    val protected_pages = ["/show_cookies.sml.*", "/email.*"]
  in
    ScsLogin.auth_filter protected_pages
  end

