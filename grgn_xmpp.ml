(*
 * (c) 2007-2010 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Transport
open XMPP
open JID
open Grgn_config
open Grgn_session

let session xmpp =
  log#info "Connected to %s!" xmpp.myjid.domain;
  ()

let run account =
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
    Transport.connect s myjid.ldomain 5222;
    XMPP.open_stream xmpp ~use_tls:false account.password session;
    let watcher _ =
      XMPP.parse xmpp;
      true
    in
    let chann = GMain.Io.channel_of_descr socket.fd in
      ignore (GMain.Io.add_watch ~cond:[`IN] ~callback:watcher chann)

let start accounts =
  List.iter run accounts
