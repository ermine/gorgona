(*
 * (c) 2005-20011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj
open GMisc
open Gobject.Data

let string_of_modifier = function
  | `BUTTON1 -> "BUTTON1"
  | `BUTTON2 -> "BUTTON2"
  | `BUTTON3 -> "BUTTON3"
  | `BUTTON4 -> "BUTTON4"
  | `BUTTON5 -> "BUTTON5"
  | `CONTROL -> "CONTROL"
  | `LOCK -> "LOCK"
  | `MOD1 -> "MOD1"
  | `MOD2 -> "MOD2"
  | `MOD3 -> "MOD3"
  | `MOD4 -> "MOD4"
  | `MOD5 -> "MOD5"
  | `SHIFT -> "SOFT"
  | _ -> "unknown"

let remove_key_bindings ~target ~sign =
  ignore (target#misc#disconnect sign)

let add_key_bindings ~target ?source keys =
  let sign =
    target#event#connect#key_press
      ~callback:(fun ev ->
        print_string (GdkEvent.Key.string ev); print_string " ";
        List.iter (fun m -> print_string (string_of_modifier m);
          print_string " | ") (GdkEvent.Key.state ev);
        print_newline ();
        let rec find_key = function
          | [] -> false
          | (modi, key, callback) :: xs ->
            if GdkEvent.Key.keyval ev = key && 
              GdkEvent.Key.state ev = modi then (
                callback (); 
                true 
              )
            else
              find_key xs
        in
          find_key keys
      )
  in
    Gaux.may ~f:(fun w -> 
      ignore (w#misc#connect#destroy 
                ~callback:(fun _ -> remove_key_bindings ~target ~sign)
      )) source

class user_input ~packing ~callback () =
  let view = GText.view ~editable:true ~wrap_mode:`WORD
    ~height:50 ~packing ~show:true () in
object (self)
  (*
  method submit = {
    GtkSignal.name = "myreturn";
    GtkSignal.classe = `widget;
    GtkSignal.marshaller = GtkSignal.marshal_unit;
  }
  *)
  initializer

  let return () =
    print_endline "here";
    let start = view#buffer#get_iter `START in
    let stop = view#buffer#get_iter `END in      
    let s = view#buffer#get_text ~start ~stop () in
    let len = String.length s in
    let s2 = 
      if len <= 0 then s
      else
        match s.[0] with
            '\n' -> String.sub s 1 (len - 1)
          | _ -> s
    in
      callback s2;
      view#buffer#delete ~start ~stop
  in
(*
  let accel_group = GtkData.AccelGroup.create () in
    view#misc#add_accelerator ~sgn:self#submit
      ~group:accel_group ~modi:[`CONTROL] GdkKeysyms._Return;
    view#misc#connect#submit ~callback ();
*)
    (*
    view#buffer#connect#changed (fun () -> changed <- true);
    *)
    add_key_bindings ~target:view [
      [`CONTROL], GdkKeysyms._Return, return;
    ]
end  
