(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)
open GObj
open GMisc
open Gobject.Data
open Grgn_xmpp
open XMPPClient
open JID
open Grgn_gtk_types

class ooc_room (appwin:appwin) person (mynick:string) packing =
  let vpaned = GPack.paned `VERTICAL ~height:200 ~packing () in
  let scrolledchat = GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`AUTOMATIC
    ~placement:`TOP_LEFT ~height:500 ~width:500 ~packing:vpaned#add1 () in
  let chatlog = GText.view ~editable:false ~wrap_mode:`WORD
    ~packing:scrolledchat#add () in
object (self)
  inherit widget vpaned#as_widget

  method send str =
    appwin#send_message ~jid_to:person ~body:str ()
    
  method mynick = mynick

  method endline () =
    let iter = chatlog#buffer#end_iter in
      chatlog#buffer#insert ~iter "\n";
      ignore (chatlog#scroll_mark_onscreen `INSERT)

  method print_text text =
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    let time =
      string_of_int tm.Unix.tm_hour ^ ":" ^ string_of_int tm.Unix.tm_min in
      
    (* let iter = chatlog#buffer#get_iter `INSERT in *)
      chatlog#buffer#insert time;
      chatlog#buffer#insert " ";
      chatlog#buffer#insert text;
      self#endline ()

  method display_message nick body =
    self#print_text ("<" ^ nick ^ "> " ^ body)

  method process_message (xmpp:Grgn_xmpp.xmpp)
    (stanza:XMPPClient.message_stanza) =
    match stanza.jid_from with
      | None -> () (* TODO *)
      | Some from ->
        match stanza.content.body with
          | None -> ()
          | Some body ->
            self#display_message from.node body

  method process_presence xmpp stanza =
    ()

  method as_page_window = (self :> page_window)

  initializer
  let _input = new Grgn_gtk_input.user_input self ~packing:vpaned#add2 () in ()
end

let start_ooc appwin person =
  let mynick = "gorgona" in
  let room_window = new ooc_room appwin person mynick
    (fun w -> appwin#add_page w) in
    appwin#add_page_window (Grgn_xmpp.lpair person) room_window#as_page_window;
    room_window#as_page_window
