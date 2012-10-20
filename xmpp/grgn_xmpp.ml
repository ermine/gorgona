(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open JID
open Xml
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

module XMPPClient = XMPP.Make (UnitMonad) (Xmlstream.XmlStreamIE) (IDCallback)
open XMPPClient

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
          
let message_callback xmpp stanza =
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some from ->
      let p =
        try Some (Hashtbl.find xmpp.user_data.pages (lpair from))
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

let presence_callback (xmpp:user_data XMPPClient.session_data) stanza =
  print_endline "presence_callback";
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some jid_from ->
      let p =
        try Some (Hashtbl.find xmpp.user_data.pages (lpair jid_from))
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
    (parse_message ~callback:message_callback ~callback_error:message_error);
  XMPPClient.register_stanza_handler xmpp (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error);

  let open R in
    get xmpp (fun ?jid_from ?jid_to ?lang ?ver items ->
      List.iter (fun item ->
        Printf.printf "%s %s\n" (JID.string_of_jid item.jid) item.name;
        flush stdout
      ) items
    )
        

module SimpleTransport =
struct
  type 'a z = 'a UnitMonad.t
  type fd = unit
      
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
end

let connect account =
  let myjid =
    if account.resource = "" then
      account.jid
    else
      replace_resource account.jid account.resource
  in
  let user_data = {
    skey = "skey";
    pages = Hashtbl.create 10
  } in
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
  let socket_module =
    if account.rawxml_log = "" then
      (module PlainSocket : XMPPClient.Socket)
    else
      let module Socket_module =
          struct
            include LogTraffic(PlainSocket)
              (struct let logfile = open_out account.rawxml_log end)
          end in
        (module Socket_module : XMPPClient.Socket)
  in
  let xmpp = create_session_data socket_module myjid user_data in
    fd, xmpp
    
let create_parser session_data account =
  let lang = account.Grgn_config.lang in
  let password = account.password in
    XMPPClient.send session_data
      (Xmlstream.stream_header session_data.ser
         (ns_streams, "stream")
         (make_attr "to" session_data.XMPPClient.myjid.domain ::
            make_attr "version" "1.0" ::
            (match lang with
              | None -> []
              | Some v -> [make_attr ~ns:ns_xml "lang" v]))) >>=
      fun () ->
     start_stream session_data
    (* ?tls:(match tls_socket with
       | None -> None
       | Some socket -> Some (fun session_data ->
       socket () >>= fun socket ->
       session_data.socket <- socket;
       let read buf start len =
       let module S = (val socket : Socket) in
       S.read S.socket buf start len
       in
       X.reset session_data.p (Some read);
       return ()
       )) *)
      lang password session >>=
      fun () -> return (fun () ->
        X.parse session_data.p
          stream_start (stream_stanza session_data) stream_end)
  
  
