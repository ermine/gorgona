(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open Xml
open JID
open Grgn_config

module UnitMonad =
struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f = f v
  let fail exn = raise exn
  let catch f1 f2 = try f1 () with exn -> f2 exn
end    

module ID =
struct
  type t = string
  let compare = Pervasives.compare
end
module IDCallback =
struct
  module T = Treap.Map(ID)
  type 'a t = 'a T.t
  let empty = T.empty
  let add key value t = T.add t key value 1
  let remove key t = T.delete t key
  let find key t = fst (T.find t key)
end

module PseudoSocket =
struct
  type t = unit
  type 'a z = 'a UnitMonad.t
  let socket = ()
  let read s buf start len = 0
  let write s str = ()
  let close s = ()
end
      
module SimpleTransport =
struct
  type 'a z = 'a UnitMonad.t
      
  type socket = {
    inc : in_channel;
    outc : out_channel;
  }

  let get_fd s = Unix.descr_of_in_channel s.inc

  let can_tls = false
  let can_compress = false

  let open_connection sockaddr =
    let inc, outc = Unix.open_connection sockaddr in
      {inc;
       outc
      }

  let read s buf start len =
    let size = input s.inc buf start len in
      size

  let write s str =
    output_string s.outc str;
    flush s.outc

  let close s = close_in s.inc; close_out s.outc

end

module XMPPClient = XMPP.Make (UnitMonad) (Xmlstream.XmlStreamIE) (IDCallback)
open XMPPClient

module LogTraffic  (T : XMPPClient.Socket)
  (L : sig val logfile : out_channel end) =
struct
  open UnitMonad

  type t = T.t
  let socket = T.socket

  let read s buf start len =
    let size = T.read s buf start len in
      if size = 0 then (
        output_string L.logfile "IN CLOSED\n";
        flush L.logfile;
        size
      ) else (
        output_string L.logfile "IN: ";
        output_string L.logfile (String.sub buf start size);
        output_string L.logfile "\n";
        flush L.logfile;
        size
      )

  let write s str =
    output_string L.logfile "OUT: ";
    output_string L.logfile str;
    output_string L.logfile "\n";
    flush L.logfile;
    T.write s str

  let close = T.close
end

let opt_try f x = try Some (f x) with Not_found -> None
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
  type xmpp = user_data XMPPClient.session_data
  and  user_data = {
    skey : string;
    pages : (string * string, A.page_window) Hashtbl.t
  }
end =
struct
  type xmpp = user_data XMPPClient.session_data
  and  user_data = {
    skey : string;
    pages : (string * string, A.page_window) Hashtbl.t
  }
end
  
open B

let add_page_window xmpp hint page =
  Hashtbl.add xmpp.user_data.pages hint page

let remove_page_window xmpp hint =
  Hashtbl.remove xmpp.user_data.pages hint

type xmpp = B.xmpp

module R = Roster.Make (XMPPClient)
module Disco = XEP_disco.Make (XMPPClient)
module XVersion = XEP_version.Make (XMPPClient)

type resource_data = {
  category : Disco.category;
(*
  presence_status : string;
  presence_chow : XMPPClient.presence_show
*)
}

type roster_data = {
  resources : (string, resource_data) Hashtbl.t
}

(*
let add_roster xmpp jid category =
  let data =
    try Some (Hashtbl.find xmpp.user_data.roster (lpair jid))
    with Not_found -> None in
  let data =
    match data with
      | None ->
        let data = {resources = Hashtbl.create 10} in
          Hashtbl.add xmpp.user_data.roster (lpair jid) data;
          data
      | Some data -> data in
    Hashtbl.add data.resources jid.lresource {category}

let remove_roster xmpp jid =
  if jid.lresource = "" then
    Hashtbl.remove xmpp.user_data.roster (lpair jid)
  else
    let data = Hashtbl.find xmpp.user_data.roster (lpair jid) in
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
          
class type account_window =
object
  method process_message : xmpp -> message_content stanza -> unit
  method process_presence : xmpp -> presence_content stanza -> unit
  method process_message_error : xmpp -> ?id:XMPP.id ->
    ?jid_from:JID.t -> ?jid_to:string -> ?lang:string -> StanzaError.t -> unit
  method process_presence_error : xmpp -> ?id:XMPP.id ->
    ?jid_from:JID.t -> ?jid_to:string -> ?lang:string -> StanzaError.t -> unit
end

let session_handler (aw:account_window) xmpp =
  XMPPClient.register_iq_request_handler xmpp XVersion.ns_version
    XVersion.(
      iq_request
        ~get:(fun ?jid_from ?jid_to ?lang () -> {
          name = Version.name;
          version = Version.version;
          os = Sys.os_type
        })
    );
  XMPPClient.register_stanza_handler xmpp (ns_client, "message")
    (parse_message ~callback:aw#process_message
       ~callback_error:aw#process_message_error);
  XMPPClient.register_stanza_handler xmpp (ns_client, "presence")
    (parse_presence ~callback:aw#process_presence
       ~callback_error:aw#process_presence_error);
  let open R in
    get xmpp (fun ?jid_from ?jid_to ?lang ?ver items ->
      List.iter (fun item ->
        Printf.printf "%s %s\n" (JID.string_of_jid item.jid) item.name;
        flush stdout
      ) items
    )

let create_session_data account =
  let user_data = {
    skey = "skey";
    pages = Hashtbl.create 10
  } in
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
    XMPPClient.create_session_data (module PseudoSocket : XMPPClient.Socket)
      myjid user_data
  
let connect account =
  let host, port =
    (if account.ip = "" then account.jid.domain else account.ip),
    (match account.port with
       | None -> 5222
       | Some i -> i
    )
  in
  let inet_addr =
    try Unix.inet_addr_of_string host
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let socket_data = SimpleTransport.open_connection sockaddr in
  let fd = SimpleTransport.get_fd socket_data in
  let module PlainSocket =
      struct
        type t = SimpleTransport.socket
        let socket = socket_data
        include SimpleTransport
      end in
    fd, (module PlainSocket : XMPPClient.Socket)

let open_stream account session_data socket session =
  let socket_module =
    if account.rawxml_log = "" then
      socket
    else
      let module S = (val socket : XMPPClient.Socket) in
      let module Socket_module =
          struct
            include LogTraffic(S)
              (struct let logfile = open_out account.rawxml_log end)
          end in
        (module Socket_module : XMPPClient.Socket)
  in
  let module S = (val socket_module : XMPPClient.Socket) in
    session_data.socket <- socket_module;
    let read buf start len = S.read S.socket buf start len in
      XMPPClient.X.reset session_data.p (Some read);
      XMPPClient.open_stream session_data
        ?lang:account.Grgn_config.lang account.Grgn_config.password session

let create_parser session_data =
  XMPPClient.parse session_data

let close session_data =
  let module S = (val session_data.socket : XMPPClient.Socket) in
    S.close S.socket

