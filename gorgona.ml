(*
 * (c) 2005-20011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Grgn_app

let create_menu appwin (main, menu_items) =
  let mainitem = GMenu.menu_item ~label:main () in
  let submenu = GMenu.menu () in
    List.iter (fun (i, a, callback) ->
      let item = GMenu.menu_item ~label:i ~packing:submenu#append () in
        item#connect#activate ~callback;
        item#add_accelerator ~group:appwin#accel_group ~flags:[`VISIBLE] a
    ) menu_items;
    mainitem#set_submenu submenu;
    appwin#menubar#append mainitem
 
let create_menus appwin =
  List.iter (fun m -> create_menu appwin m) [
    "Gorgona", ["Quit", GdkKeysyms._Q, GMain.Main.quit];
    "Services", ["Join Conference", GdkKeysyms._J, Grgn_muc.join]
  ]

let _ =
  create_menus appwin;
  appwin#activate ();

    (* GtkThread.main () *)
  GMain.Main.main ()
