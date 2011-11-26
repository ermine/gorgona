(*
 * (c) 2005-2011 Anastasia Gornostaeva
 *)

open GObj
open GMisc
open Gobject.Data

open Grgn_muc
open Grgn_gtk_app

let tag_muc_event = "muc-event"

class muc_roster ?packing () =
  let cols = new GTree.column_list in
  let _col_item = cols#add string in
  let model = GTree.tree_store cols in
  let view = GTree.view ~model ?packing () in
object (self)
  inherit widget view#as_widget
end

let send_user_input room body =
  XMPP.send_message appwin#xmpp ~jid_to:room ~kind:XMPP.Groupchat ~body ()

class muc_room room_name mynick ?packing () =
  let vpaned = GPack.paned `VERTICAL ?packing () in
  let hpaned = GPack.paned `HORIZONTAL ~height:200 ~packing:vpaned#add1 () in
  let scrolledchat = GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`AUTOMATIC
    ~placement:`TOP_LEFT ~height:500 ~width:500 ~packing:hpaned#add1 () in
  let chatlog = GText.view ~editable:false ~wrap_mode:`WORD
    ~packing:scrolledchat#add () in
  let _input = new Grgn_gtk_input.user_input ~packing:vpaned#add2
    ~callback:(fun text -> send_user_input room_name text) () in
  let scrolledroster = GBin.scrolled_window ~hpolicy:`AUTOMATIC
    ~vpolicy:`AUTOMATIC ~placement:`TOP_LEFT ~packing:hpaned#add2 () in
  let _roster = new muc_roster ~packing:scrolledroster#add () in
    
object (self)
  inherit widget vpaned#as_widget
    
  val mutable mynick = ref mynick
  val occupants : (string, occupant) Hashtbl.t = Hashtbl.create 20

  method get_mynick = !mynick
  method set_mynick nick = mynick := nick

  method occupants = occupants

  method add_occupant nick data =
    Hashtbl.add occupants nick data
      
  method find_occupant nick =
    Hashtbl.find occupants nick

  method mem_occupant nick =
    Hashtbl.mem occupants nick

  method remove_occupant nick =
    Hashtbl.remove occupants nick
    
  method endline () =
    let iter = chatlog#buffer#end_iter in
      chatlog#buffer#insert ~iter "\n";
      ignore (chatlog#scroll_mark_onscreen `INSERT)

  method print_text tag text =
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    let time =
      string_of_int tm.Unix.tm_hour ^ ":" ^ string_of_int tm.Unix.tm_min in
      
    (* let iter = chatlog#buffer#get_iter `INSERT in *)
      chatlog#buffer#insert time;
      chatlog#buffer#insert " ";
      chatlog#buffer#insert text;
      self#endline ()

  method muc_event_join nick =
    self#print_text tag_muc_event (nick ^ " had entered the room")

  method muc_event_leave nick (reason:string option) =
    self#print_text tag_muc_event (nick ^ "had left the room")

  method muc_event_nick_changed (oldnick:string)  (newnick:string)
    (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_destroy (jid:JID.t option) (password:string option) (reason:string option) () =
    self#print_text tag_muc_event "smth"
      
  method muc_event_ban (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_kick (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_room_created () =
    self#print_text tag_muc_event "smth"

  method muc_event_affiliation (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_members_only (reason:string option) =
    self#print_text tag_muc_event "smth"
      
  method muc_event_system_shutdown (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_decline (jid1:JID.t option) (jid2:JID.t option)
    (reason:string option) () =
    self#print_text tag_muc_event "smth"

  method muc_event_invite (jid1:JID.t option) (jid2:JID.t option)
    (reason:string option) (password:string option) () =
    self#print_text tag_muc_event "smth"

  method process_presence xmpp stanza =
    Grgn_muc.process_presence self xmpp stanza

  method process_message (xmpp:Grgn_xmpp.xmpp) (stanza:XMPP.message_stanza) =
    Grgn_muc.process_message self xmpp stanza

  method as_page_window = (self :> page_window)

  method display_message nick body =
    self#print_text tag_muc_event ("<" ^ nick ^ "> " ^ body)

end

(* menu callback for "Join Conference" *)
let join () =
  let mynick = "gorgona" in
  let room = JID.of_string "devel@conference.jabber.ru" in
  let room_window = new muc_room room mynick
    ~packing:(fun w -> appwin#add_page w) () in
    appwin#add_page_window (Grgn_xmpp.lpair room) room_window#as_page_window;
    XEP_muc.enter_room appwin#xmpp ~nick:mynick room
