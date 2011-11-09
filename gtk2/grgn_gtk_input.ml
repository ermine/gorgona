(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj

class user_input ~packing ~callback () =
  let view = GText.view ~editable:true ~wrap_mode:`WORD
    ~height:50 ~packing ~show:true () in
object (self)
  inherit widget view#as_widget

  initializer
  let return () =
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
      view#buffer#delete ~start ~stop;
      GtkSignal.stop_emit ()
  in
    (*
    view#buffer#connect#changed (fun () -> changed <- true);
    *)

  let sign =
    view#event#connect#key_press
      ~callback:(fun ev ->
        if GdkEvent.Key.keyval ev = GdkKeysyms._Return &&
          (not (List.mem `CONTROL (GdkEvent.Key.state ev)) ||
             not (List.mem `CONTROL (GdkEvent.Key.state ev))) then (
            return ();
            true
           ) else
          false
      )
  in
    ()
    
end  
