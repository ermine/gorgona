(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GMain

let _ =
  GtkMain.Main.init ()

let _ =
  let window = GWindow.window ~width:200 ~height:200 () in
  let top = GPack.hbox ~packing:window#add () in
  let _label = GMisc.label ~text:"Test" ~packing:top#add () in
    window#event#connect#delete 
      ~callback:(fun _ -> prerr_endline "Delete event occured"; false);
    window#connect#destroy ~callback:Main.quit;

    window#show ();
    (* GtkThread.main () *)
    Main.main ()
