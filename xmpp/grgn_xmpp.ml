(*
 * (c) 2005-2011 Anastasia Gornostaeva
 *)

open Transport
open XMPP
open JID
open Xml
open Grgn_config

let catch f x = try Some (f x) with Not_found -> None
let lpair jid = (jid.lnode, jid.ldomain)

module rec A : sig
  class type page_window =
  object
    method process_message : B.xmpp -> message_stanza -> unit
    method process_presence : B.xmpp -> presence_stanza -> unit
  (* method iq_callback : xmpp  *)
  end
end =    
struct
  class type page_window =
  object
    method process_message : B.xmpp -> message_stanza -> unit
    method process_presence : B.xmpp -> presence_stanza -> unit
  (* method iq_callback : xmpp  *)
  end
end
and B : sig
  type xmpp = session_data XMPP.t
  and  session_data = {
    skey : string;
    pages : (string * string, A.page_window) Hashtbl.t
  }
end =
    struct
      type xmpp = session_data XMPP.t
      and  session_data = {
        skey : string;
        pages : (string * string, A.page_window) Hashtbl.t
      }
    end

open B

let add_page_window xmpp hint page =
  Hashtbl.add xmpp.data.pages hint page

let remove_page_window xmpp hint =
  Hashtbl.remove xmpp.data.pages hint

type xmpp = B.xmpp
    
type resource_data = {
  category : XEP_disco.category;
  (*
  presence_status : string;
  presence_chow : XMPP.presence_show
  *)
}

type roster_data = {
  resources : (string, resource_data) Hashtbl.t
}

(*
let add_roster xmpp jid category =
  let data =
    try Some (Hashtbl.find xmpp.data.roster (lpair jid))
    with Not_found -> None in
  let data =
    match data with
      | None ->
        let data = {resources = Hashtbl.create 10} in
          Hashtbl.add xmpp.data.roster (lpair jid) data;
          data
      | Some data -> data in
    Hashtbl.add data.resources jid.lresource {category}

let remove_roster xmpp jid =
  if jid.lresource = "" then
    Hashtbl.remove xmpp.data.roster (lpair jid)
  else
    let data = Hashtbl.find xmpp.data.roster (lpair jid) in
      Hashtbl.remove data.resources jid.lresource

let get_data roster jid =
  let data =
    try Some (Hashtbl.find roster (lpair jid))
    with Not_found -> None in
    match data with
      | None -> None
      | Some data ->
        try Some (Hashtbl.find data.resources jid.lresource).category
        with Not_found -> None
*)
    
(* let iq_extensions = Hashtbl.create 100
let message_extensions = Hashtbl.create 100
let presence_extensions = Hashtbl.create 100
*)
    
(*
let add_message_extension ns proc =
  Hashtbl.add message_extensions ns proc

let add_presence_extension ns proc =
  Hashtbl.add presence_extensions ns proc
*)
          
let message_callback xmpp stanza =
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some from ->
      let p =
        try Some (Hashtbl.find xmpp.data.pages (lpair from))
        with Not_found -> None in
        match p with
          | None -> () (* TODO *)
          | Some p ->
            p#process_message xmpp stanza

(*      
      let rec iter_x = function
        | [] -> true
        | x :: xs ->
          match x with
            | Xmlelement (qname, _, _) ->
              let f =
                try Some (Hashtbl.find message_extensions qname)
                with Not_found -> None in
                match f with
                  | None -> iter_x xs
                  | Some f ->
                    if f xmpp stanza x then
                      iter_x xs
                    else
                      false
      in
      let result = iter_x stanza.x in
        if result then
          (* display message body and get x:delay *)
          print_endline "body"
*)
              
let message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let presence_callback (xmpp:session_data XMPP.t) stanza =
  print_endline "presence_callback";
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some jid_from ->
      let p =
        try Some (Hashtbl.find xmpp.data.pages (lpair jid_from))
        with Not_found -> None in
        match p with
          | None -> () (* TODO *)
          | Some p ->
            print_endline "presence";
            p#process_presence xmpp stanza

(*              
      let rec iter_x = function
        | [] -> false
        | x :: xs ->
          match x with
            | Xmlelement (qname, _, _) ->
              let f =
                try Some (Hashtbl.find presence_extensions qname)
                with Not_found -> None in
                match f with
                  | None -> iter_x xs
                  | Some f ->
                    if f xmpp stanza x then
                      iter_x xs
                    else
                      false
      in
      let _ = iter_x stanza.x in
        ()
*)
              
let presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let session xmpp =
  XMPP.register_iq_request_handler xmpp XEP_version.ns_version
    XEP_version.(
      iq_request
        ~get:(fun ?jid_from ?jid_to ?lang () -> {
          name = Version.name;
          version = Version.version;
          os = Sys.os_type
        })
    );
  XMPP.register_stanza_handler xmpp (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  XMPP.register_stanza_handler xmpp (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error);

  let open Roster in
        get xmpp (fun ?jid_from ?jid_to ?lang ?ver items ->
          List.iter (fun item ->
            Printf.printf "%s %s\n" (JID.string_of_jid item.jid) item.name;
            flush stdout
          ) items
        )
        

let create_account account =
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
  let session_data = {
    skey = "skey";
    pages = Hashtbl.create 10
  } in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
} in
  let xmpp = XMPP.create session_data socket myjid in
    xmpp

let connect xmpp account =
  Transport.connect xmpp.socket.fd xmpp.myjid.ldomain 5222;
  XMPP.open_stream xmpp ~use_tls:false account.password session
