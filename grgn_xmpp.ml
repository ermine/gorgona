(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Transport
open XMPP
open JID
open Grgn_config
open Grgn_session

let catch f x = try Some (f x) with Not_found -> None
let lpair jid = (jid.lnode, jid.ldomain)

module Chats =
  Map.Make(struct type t = string * string let compare = Pervasives.compare end)

module ResourceSet = Set.Make(String)

type ctx = session_data XMPP.t
and session_data = {
  skey : string;
  mutable chats : chat_data Chats.t
}
and chat_data = {
  mutable resources : ResourceSet.t;
  presence_handler : (ctx -> presence_content stanza -> unit);
  message_handler : (ctx -> message_content stanza -> unit)
}

let message_callback xmpp stanza =
  ()
    (*
  let hook =
    try Some (Chats.find stanza.jid_from xmpp.chats)
    with Not_found -> None in
    match hook with
      | Some f -> f xmpp stanza
      | None -> ()
    *)

let add_hook xmpp jid presence_proc message_proc =
  let data =
    try Some (Chats.find (lpair jid) xmpp.data.chats)
    with Not_found -> None in
    match data with
      | Some data ->
        if not (ResourceSet.mem jid.lresource data.resources ) then
          data.resources <- ResourceSet.add jid.lresource data.resources
      | None ->
        xmpp.data.chats <- Chats.add (lpair jid) {
          resources = ResourceSet.add jid.lresource ResourceSet.empty;
          presence_handler = presence_proc;
          message_handler = message_proc
        } xmpp.data.chats
        
let add_resource xmpp jid =
  let data =
    try Some (Chats.find (lpair jid) xmpp.data.chats)
    with Not_found -> None in
    match data with
      | Some data ->
        if not (ResourceSet.mem jid.lresource data.resources ) then
          data.resources <- ResourceSet.add jid.lresource data.resources
      | None -> ()
        
let remove_hook xmpp jid =
  let data =
    try Some (Chats.find (lpair jid) xmpp.data.chats)
    with Not_found -> None in
    match data with
      | Some data ->
        if jid.lresource <> "" then
          data.resources <- ResourceSet.remove jid.lresource data.resources
        else
          xmpp.data.chats <- Chats.remove (lpair jid) xmpp.data.chats
      | None -> ()

let remove_resource xmpp jid =
  let data =
    try Some (Chats.find (lpair jid) xmpp.data.chats)
    with Not_found -> None in
    match data with
      | Some data ->
        if jid.lresource <> "" then
          data.resources <- ResourceSet.remove jid.lresource data.resources
      | None -> ()
  
let message_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let presence_callback (xmpp:session_data XMPP.t) stanza =
  match stanza.jid_from with
    | Some jid_from -> (
      let data =
        try Some (Chats.find (lpair jid_from) xmpp.data.chats)
        with Not_found -> None in
        match data with
          | Some data -> data.presence_handler xmpp stanza
          | None -> ()
    )
    | None -> ()
    

let presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
  ()

let session xmpp =
  XMPP.register_iq_request_handler xmpp XEP_version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
      match ev with
        | IQGet _el ->
          let el = XEP_version.encode {XEP_version.name = Version.name;
                                       XEP_version.version = Version.version;
                                       XEP_version.os = Sys.os_type} in
            IQResult (Some el)
        | IQSet _el ->
          raise BadRequest
    );
  XMPP.register_stanza_handler xmpp (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  XMPP.register_stanza_handler xmpp (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error)

let create_account account =
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
  let session_data = {
    skey = "skey";
    chats = Chats.empty
  } in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
} in
  let xmpp = XMPP.create session_data socket myjid in
    let watcher _ =
      XMPP.parse xmpp;
      true
    in
    let chann = GMain.Io.channel_of_descr socket.fd in
      ignore (GMain.Io.add_watch ~cond:[`IN] ~callback:watcher chann);
      xmpp

let connect xmpp account =
  Transport.connect xmpp.socket.fd xmpp.myjid.ldomain 5222;
  XMPP.open_stream xmpp ~use_tls:false account.password session
