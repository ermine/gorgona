(*
 * (c) 2007-20010 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GMain

let _ =
  GtkMain.Main.init ()

let add_menu menubar =
  let gorgona_menu = GMenu.menu () in
  let item = GMenu.menu_item ~label:"Quit" ~packing:gorgona_menu#append () in
    item#connect#activate ~callback:GMain.Main.quit;
    let gorgona_item = GMenu.menu_item ~label:"Gorgona" () in
    gorgona_item#set_submenu gorgona_menu;
    menubar#append gorgona_item

let _ =
  let window = GWindow.window ~width:200 ~height:200 () in
  let _ =
    window#move ~x:20 ~y:20;
    window#event#connect#delete 
      ~callback:(fun _ -> prerr_endline "Delete event occured"; false);
    window#connect#destroy ~callback:Main.quit in

  let vbox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:vbox#add () in
  let vpaned = GPack.paned `HORIZONTAL ~packing:vbox#add () in
  let _roster = GMisc.label ~text:"Roster" ~packing:vpaned#add () in
  let notebook = GPack.notebook ~packing:vpaned#add () in
  let _tab = GMisc.label ~text:"Tab"
    ~packing:(fun w -> ignore (notebook#append_page w)) ()in

    add_menu menubar;
    window#show ();

    let accounts = Grgn_config.get_config () in
      Grgn_xmpp.start accounts;

      (* GtkThread.main () *)
      Main.main ()
