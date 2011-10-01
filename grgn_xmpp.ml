(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Transport
open XMPP
open JID
open Grgn_config
open Grgn_session

let message_callback t stanza =
  ()

let message_error t ?id ?jid_from ?jid_to ?lang error =
  ()
    
let presence_callback t stanza =
  ()
  
let presence_error t ?id ?jid_from ?jid_to ?lang error =
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
  let session_key = "skey" in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
} in
  let xmpp = XMPP.create session_key socket myjid in
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
