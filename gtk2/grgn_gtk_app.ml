(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open GObj
open GMain

open Grgn_xmpp

let _ =
  GtkMain.Main.init ()
  

class type page_window =
object
  (* method process_iq : xmpp -> XMPP.iq_stanza -> unit *)
  method process_presence : xmpp -> XMPPClient.presence_stanza -> unit
  method process_message : xmpp -> XMPPClient.message_stanza -> unit
end


class appwin data xmpp =
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
  method window = window
  method menubar = menubar
  method accel_group = accel_group
  method xmpp = xmpp

  method activate () =
    window#show ()

  method add_page_window hint page =
    Grgn_xmpp.add_page_window xmpp hint page
      
  method add_page page =
    ignore (notebook#append_page page)

end

let appwin =
  let accounts = Grgn_config.get_config () in
  let account = List.hd accounts in
  let fd, xmpp = Grgn_xmpp.connect account in
  let parse = create_parser xmpp account in
  let watcher _ =
      (* TODO modify all internal data in Glib.idle *)
    let () =
      try
        parse ()
      with exn -> print_endline (Printexc.to_string exn)
    in
      true
  in
  let chann = GMain.Io.channel_of_descr fd in
  let () =
    ignore (GMain.Io.add_watch ~cond:[`IN] ~callback:watcher chann) in
    
  let appwin = new appwin account xmpp in
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
