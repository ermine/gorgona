(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj
open GMain

let _ =
  GtkMain.Main.init ()
  
class appwin data =
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
  let _roster = new Grgn_roster.roster ~packing:vpaned#add1 () in
  let notebook = GPack.notebook ~packing:vpaned#add2 () in

  let xmpp = Grgn_xmpp.create_account data in
  
object (self)
  inherit widget window#as_widget

  method window = window
  method menubar = menubar
  method accel_group = accel_group
  method xmpp = xmpp

  method activate () =
    window#show ();
    Grgn_xmpp.connect xmpp data

  method add_chat w =
    ignore (notebook#append_page w)

end

let appwin =
  let accounts = Grgn_config.get_config () in
  let appwin = new appwin (List.hd accounts) in
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
