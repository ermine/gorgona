(*
 * (c) 2005-2011 Anastasia Gornostaeva
 *)

open Grgn_gtk_app


let _ =
  GtkMain.Rc.parse_string "
      binding \"Pouet\" {
        bind \"<Control>S\" { \"my-return\" () }
      }

class \"GtkTextView\" binding \"Pouet\"
"

let create_menus () =
  List.iter (fun m -> create_menu m) [
    "Gorgona", ["Quit", [`CONTROL], GdkKeysyms._Q, GMain.Main.quit];
    "Services", ["Join Conference", [`CONTROL], GdkKeysyms._J, Grgn_gtk_muc.join]
  ]

let _ =
  create_menus ();
  appwin#activate ();

  main ()
