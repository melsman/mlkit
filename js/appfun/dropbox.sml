(* Dropbox support using OAuth2 (implicit flow) and Dropbox API v2 *)

structure Dropbox :> DROPBOX = struct

infix *>
fun a *> b = (a,b)

type client = OAuth.client

type 'a cont = ('a -> unit) -> unit

fun client (client_id:string) =
    OAuth.client {authorize="https://www.dropbox.com/oauth2/authorize",
                  client_id=client_id,
                  redirect_uri="http://localhost:8000"}

fun authorize (c:client) : unit =
    OAuth.authorize c

fun isAuthorized (c: client) : bool =
    case OAuth.token c of
        SOME _ => true
      | NONE => false

fun signOut (c:client) (f:unit->unit) : unit =
    OAuth.logout c

local structure X = Js.XMLHttpRequest
in
fun getRequest url =
    let val r = X.new()
        val () = X.openn r {method="GET",url=url,async=false}
        val () = X.send r NONE
    in case X.response r of
           SOME res => res
         | NONE => raise Fail ("dropbox.getRequest failed: " ^ url)
    end
fun postRequest (url:string) (headers:(string*string)list) (data:string) : string =
    let val r = X.new()
        val () = X.openn r {method="POST",url=url,async=false}
        val () = List.app (X.setRequestHeader r) headers
        val () = X.send r (SOME data)
    in case X.response r of
           SOME res => res
         | NONE => raise Fail ("dropbox.postRequest failed: " ^ url)
    end
fun postRequestAsync (url:string) (headers:(string*string)list) (data:string) (cont:string->unit) : unit =
    let val r = X.new()
        val () = X.openn r {method="POST",url=url,async=true}
        val () = List.app (X.setRequestHeader r) headers
        val () = X.onStateChange r (fn () => if X.state r >= 4 then
                                                   case X.response r of
                                                       SOME res => cont res
                                                     | NONE => raise Fail ("dropbox.postRequestAsync failed: "
                                                                           ^ url)
                                             else ())
    in X.send r (SOME data)
    end
end

fun headers c =
    case OAuth.token c of
        SOME t =>
        [("Authorization", "Bearer " ^ t)]
      | NONE => []

fun headers_json c =
    headers c @ [("Content-Type", "application/json; charset=utf-8")]


fun dropboxUid (c:client) (cont:string -> unit) : unit =
    let val url = "https://api.dropboxapi.com/2/users/get_current_account"
        val data = "null"
    in postRequestAsync url (headers_json c) data (fn res =>
                                                      case Json.fromString res of
                                                          Json.OBJECT obj =>
                                                          (case Json.objLook obj "name" of
                                                               SOME (Json.OBJECT obj) =>
                                                               (case Json.objLook obj "display_name" of
                                                                    SOME (Json.STRING n) => cont n
                                                                  | _ => cont "noname1")
                                                             | _ => cont "noname2")
                                                        | _ => cont "noname3")
    end

structure FileStore = struct
  type filestore = client
  type filename = string and dirname = string
  fun filestore (c:client) : filestore = c
  fun all_files (fs:filestore) (cont:filename list -> unit) : unit =
      let val data = ["path" *> Json.STRING "",
                      "recursive" *> Json.BOOL true]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          fun cont2 (res:string) : unit =
              let val filenames =
                      case Json.fromString res of
                          Json.OBJECT obj =>
                          (case Json.objLook obj "entries" of
                               SOME(Json.ARRAY ts) =>
                               map (fn Json.OBJECT obj =>
                                       (case Json.objLook obj "path_display" of
                                            SOME (Json.STRING filename) => "Dropbox" ^ filename
                                          | _ => raise Fail "Dropbox.FileStore.all_files.no path_display")
                                   | _ => raise Fail "Dropbox.FileStore.all_files.expecting obj") ts
                             | _ => raise Fail "Dropbox.FileStore.all_files.expecing array")
                        | _ => raise Fail "Dropbox.FileStore.all_files.expecting obj2"
              in cont filenames
              end
      in postRequestAsync "https://api.dropboxapi.com/2/files/list_folder"
                          (headers_json fs) data cont2
      end

  fun strip f =
      if String.isPrefix "Dropbox/" f then String.extract(f,7,NONE)
      else f

  fun content (fs:filestore) (f:filename) (cont:string->unit) : unit =
      let val data = ["path" *> Json.STRING (strip f)]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          val hs = headers fs @ ["Dropbox-API-Arg" *> data,
                                 "Content-Type" *> "text/plain"]
      in postRequestAsync "https://content.dropboxapi.com/2/files/download"
                          hs "" cont
      end

  fun write_file (fs:filestore) (f:filename) (content:string) (cont: {msg:string}->unit) : unit =
      let val data = ["path" *> Json.STRING (strip f),
                      "mode" *> Json.STRING "overwrite"]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          val hs = headers fs @ ["Dropbox-API-Arg" *> data,
                                 "Content-Type" *> "application/octet-stream"]
          fun cont2 res =
              let val msg =
                      case Json.fromString res of
                          Json.OBJECT obj =>
                          (case Json.objLook obj "size" of
                               SOME(Json.NUMBER s) => s ^ " bytes"
                             | _ => "expecting number")
                        | _ => "expecting object"
              in cont {msg=msg}
              end
      in postRequestAsync "https://content.dropboxapi.com/2/files/upload"
                          hs content cont2
      end

  fun mkdir (fs:filestore) (d:dirname) (cont:{msg:string} -> unit) : unit =
      let val hs = headers fs @ ["Content-Type" *> "application/json"]
          val data = ["path" *> Json.STRING (strip d)]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          fun cont2 res =
              let val msg =
                      case Json.fromString res of
                          Json.OBJECT obj =>
                          (case Json.objLook obj "metadata" of
                               SOME(Json.OBJECT obj) =>
                               (case Json.objLook obj "name" of
                                    SOME(Json.STRING n) => "name " ^ n
                                  | _ => "expecting string")
                             | _ => "expecting object")
                        | _ => "expecting object0"
              in cont {msg=msg}
              end
      in postRequestAsync "https://api.dropboxapi.com/2/files/create_folder_v2"
                          hs data cont2
      end

  fun delete_file (fs:filestore) (f:filename) (cont:{msg:string}->unit) : unit =
      let val hs = headers fs @ ["Content-Type" *> "application/json"]
          val data = ["path" *> Json.STRING (strip f)]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          fun cont2 res =
              let val msg =
                      case Json.fromString res of
                          Json.OBJECT obj =>
                          (case Json.objLook obj "metadata" of
                               SOME(Json.OBJECT obj) =>
                               (case Json.objLook obj "name" of
                                    SOME(Json.STRING n) =>
                                    (case Json.objLook obj ".tag" of
                                         SOME (Json.STRING kind) => kind ^ " " ^ n
                                       | _ => "name " ^ n)
                                  | _ => "expecting string")
                             | _ => "expecting object")
                        | _ => "expecting object0"
              in cont {msg=msg}
              end
      in postRequestAsync "https://api.dropboxapi.com/2/files/delete_v2"
                          hs data cont2
      end

  fun delete_dir (fs:filestore) (d:dirname) (cont:{msg:string}->unit) : unit =
      delete_file fs d cont

  fun move_file (fs:filestore) (f:filename) (fnew:filename) (cont:{msg:string}->unit) : unit =
      let val hs = headers fs @ ["Content-Type" *> "application/json"]
          val data = ["from_path" *> Json.STRING (strip f),
                      "to_path" *> Json.STRING (strip fnew)]
          val data = Json.toString(Json.OBJECT(Json.objFromList data))
          fun cont2 res =
              let val msg =
                      case Json.fromString res of
                          Json.OBJECT obj =>
                          (case Json.objLook obj "metadata" of
                               SOME(Json.OBJECT obj) =>
                               (case Json.objLook obj "name" of
                                    SOME(Json.STRING n) =>
                                    (case Json.objLook obj ".tag" of
                                         SOME (Json.STRING kind) => kind ^ " " ^ n
                                       | _ => "name " ^ n)
                                  | _ => "expecting string")
                             | _ => "expecting object")
                        | _ => "expecting object0"
              in cont {msg=msg}
              end
      in postRequestAsync "https://api.dropboxapi.com/2/files/move_v2"
                          hs data cont2
      end

  fun move_dir (fs:filestore) (f:filename) (fnew:filename) (cont:{msg:string}->unit) : unit =
      move_file fs f fnew cont

end

end
