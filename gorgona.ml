(*
 * (c) 2005-20011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Grgn_app

let create_menus () =
  List.iter (fun m -> create_menu m) [
    "Gorgona", ["Quit", [`CONTROL], GdkKeysyms._Q, GMain.Main.quit];
    "Services", ["Join Conference", [`CONTROL], GdkKeysyms._J, Grgn_muc.join]
  ]

let _ =
  create_menus ();
  appwin#activate ();

  main ()
