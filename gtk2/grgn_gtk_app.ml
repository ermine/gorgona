(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open GObj
open GMain
open Grgn_gtk_types

open Grgn_xmpp
open JID
open XMPPClient

let _ =
  GtkMain.Main.init ()

class appwin account session_data =
  let window = GWindow.window ~width:400 ~height:400 () in
  let vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let accel_group = GtkData.AccelGroup.create () in
  let _ =
    window#move ~x:200 ~y:200;
    ignore (window#event#connect#delete 
              ~callback:(fun _ -> prerr_endline "Delete event occured"; false));
    ignore (window#connect#destroy ~callback:Main.quit);
    window#add_accel_group accel_group;
  in
  let vpaned = GPack.paned `HORIZONTAL
    ~packing:(vbox#pack ~expand:true ~fill:true) () in
  let _roster = new Grgn_gtk_roster.roster ~packing:vpaned#add1 () in
  let notebook = GPack.notebook ~packing:vpaned#add2 () in

object (self)
  val xmpp : xmpp = session_data
  method xmpp = xmpp

  method window = window
  method menubar = menubar
  method accel_group = accel_group

  method activate () =
    window#show ()

  method add_page_window hint page =
    Grgn_xmpp.add_page_window xmpp hint page
      
  method add_page page =
    ignore (notebook#append_page page)

  method send_message =
    XMPPClient.send_message xmpp

  method as_appwin = (self :> Grgn_gtk_types.appwin)
    
  method process_message (session_data:xmpp) stanza =
    match stanza.jid_from with
      | None -> () (* TODO *)
      | Some from ->
        let p =
          try Some (Hashtbl.find xmpp.user_data.B.pages (lpair from))
          with Not_found -> None in
          match p with
            | None ->
              let p = Grgn_gtk_ooc.start_ooc self#as_appwin from in
                p#process_message xmpp stanza
            | Some p ->
              p#process_message xmpp stanza

  method process_message_error xmpp ?(id:XMPP.id option) ?(jid_from:JID.t option)
    ?(jid_to:string option) ?(lang:string option) (error:StanzaError.t) =
    ()
                
  method process_presence xmpp stanza =
    match stanza.jid_from with
      | None -> () (* TODO *)
      | Some jid_from ->
        let p =
          try Some (Hashtbl.find xmpp.user_data.B.pages (lpair jid_from))
          with Not_found -> None in
          match p with
            | None -> () (* TODO *)
            | Some p ->
              print_endline "presence";
              p#process_presence xmpp stanza

  method process_presence_error xmpp ?id ?jid_from ?jid_to ?lang error =
    ()
  method xmpp_connect () =
    let fd, socket = Grgn_xmpp.connect account in
    let session = Grgn_xmpp.session_handler (self :> account_window) in
      Grgn_xmpp.open_stream account self#xmpp socket session;
      let watcher _ =
      (* TODO modify all internal data in Glib.idle *)
        try
          parse session_data;
          true
        with
          | End_of_file ->
            Grgn_xmpp.close session_data;
            false
          | XMPPClient.X.XmlError _ ->
            false
          | exn ->
            print_endline (Printexc.to_string exn);
            true
      in
      let chann = GMain.Io.channel_of_descr fd in
        ignore (GMain.Io.add_watch ~cond:[`IN] ~callback:watcher chann)
end


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
              
let appwin =
  let accounts = Grgn_config.get_config () in
  let account = List.hd accounts in
  let xmpp = Grgn_xmpp.create_session_data account in
  let appwin = new appwin account xmpp in
    appwin#xmpp_connect ();
    appwin

let create_menu (main, menu_items) =
  let mainitem = GMenu.menu_item ~label:main () in
  let submenu = GMenu.menu () in
    List.iter (fun (i, modi, key, callback) ->
      let item = GMenu.menu_item ~label:i ~packing:submenu#append () in
        item#connect#activate ~callback;
        item#add_accelerator ~group:appwin#accel_group
          ~modi ~flags:[`VISIBLE] key
    ) menu_items;
    mainitem#set_submenu submenu;
    appwin#menubar#append mainitem
      
let main () =
  (* GtkThread.main () *)
  GMain.Main.main ()
