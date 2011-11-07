(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj
open GMisc
open Gobject.Data

open Grgn_app
  
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

class muc_room room_name ?packing () =
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

  method muc_event_decline (jid1:JID.t) (jid2:JID.t) (reason:string option) =
    self#print_text tag_muc_event "smth"

  method muc_event_invite (jid1:JID.t) (jid2:JID.t) (reason:string option)
    (password:string option) () =
    self#print_text tag_muc_event "smth"
    

end

open XMPP
open JID
open XEP_muc

type occupant = {
  (* nick : string; *)
  mutable jid : JID.t option;
  mutable affiliation : affiliation;
  mutable role : role
}

module Occupant = Map.Make(String)

type room_ctx = {
  room_widget : muc_room;
  mutable mynick : string;
  mutable occupants : occupant Occupant.t
}
  
type reason = string
type password = string

let get_reason = function
  | None -> None
  | Some i -> i.User.reason
  
let process_presence_user ctx xmpp stanza from data enter =
  let () =
    match data.User.item with
      | None -> ()
      | Some item ->
        if stanza.content.presence_type = None then
          let occupant =
            Occupant.find from.lresource ctx.occupants in
            (match item.User.jid with
              | None -> ()
              | Some jid -> occupant.jid <- Some jid);
            (match item.User.role with
              | None -> ()
              | Some r -> occupant.role <- r);
            (match item.User.affiliation with
              | None -> ()
              | Some a -> occupant.affiliation <- a)
  in
  let removal =
    match data.User.destroy with
      | None -> false
      | Some (venue, reason) ->
        if stanza.content.presence_type = Some Unavailable then (
          ctx.room_widget#muc_event_destroy venue data.User.password reason ();
          true
        ) else
          false
  in
  let removal = List.fold_left
    (fun removal -> function
      | 100 ->
           (* context: Entering a room *)
           (* Inform user that any occupant is allowed to see the user's
              full JID *)
        removal
      | 110 ->
           (* context Any room presence *)
           (* Inform user that presence refers to one of its own
              room occupants *)
        removal
      | 170 ->
           (* context Configuration change *)
           (* Inform occupants that room logging is now enabled *)
        removal
      | 201 ->
           (* context Entering a room *)
           (* Inform user that a new room has been created *)
        if enter then
          ctx.room_widget#muc_event_room_created ();
        removal
      | 210 ->
           (* context Entering a room *)
           (* Inform user that service has assigned or modified
              occupant's roomnick *)
        if enter then
          ctx.mynick <- from.lresource;
        removal
      | 301 ->
           (* context Removal from room *)
           (* Inform user that he or she has been banned from the room *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            ctx.room_widget#muc_event_ban reason;
            true
        else
          removal
      | 303 -> (
           (* context Exiting a room *)
           (* Inform all occupants of new room nickname *)
        if stanza.content.presence_type = Some Unavailable then
          match data.User.item with
            | None -> removal
            | Some i ->
              match i.User.nick with
                | None -> removal
                | Some newnick ->
                  if from.lresource = ctx.mynick then
                    ctx.mynick <- newnick;
                  ctx.occupants <- Occupant.add newnick
                    (Occupant.find from.lresource ctx.occupants)
                    ctx.occupants;
                  let reason = get_reason data.User.item in
                    ctx.room_widget#muc_event_nick_changed
                      from.resource newnick reason;
                    true
        else
          removal
      )
      | 307 -> (
           (* context Removal from room *)
           (* Inform user that he or she has been kicked from the room *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            ctx.room_widget#muc_event_kick reason;
            true
        else
          removal
      )
      | 321 -> (
        (* context Removal from room *)
        (* Inform user that he or she is being removed from
           the room because of an affiliation change *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            ctx.room_widget#muc_event_affiliation reason;
            true
        else
          removal
      )
      | 322 -> (
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because the room has been changed to
              members-only and the user is not a member *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            ctx.room_widget#muc_event_members_only reason;
            true
        else
          removal
      )
      | 332 ->
           (* context Removal from room *)
           (* Inform user that he or she is being removed from the room
              because of a system shutdown *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            ctx.room_widget#muc_event_system_shutdown reason;
            true
        else
          removal
      | _ ->
        removal
    ) removal data.User.status in
    if stanza.content.presence_type = Some Unavailable && not removal then
      ctx.room_widget#muc_event_leave from.resource stanza.content.status
        
let process_presence_x ctx xmpp stanza from enter =
  List.iter (function
    | Xml.Xmlelement (qname, _, _) as el ->
      if qname = (ns_muc_user, "x") then
        process_presence_user ctx xmpp stanza from (User.decode el) enter
    | _ ->
      ()
  ) stanza.x
 
  
let process_presence ctx xmpp stanza =
  match stanza.jid_from with
    | None -> ()
    | Some from ->
      match stanza.content.presence_type with
        | None
        | Some Unavailable ->
          let enter =
            if stanza.content.presence_type = None &&
              not (Occupant.mem from.lresource
                     ctx.occupants) then
              true
            else
              false
          in
            if enter then
              ctx.occupants <- Occupant.add from.lresource
                {jid = None;
                 affiliation = AffiliationNone;
                 role = RoleNone} ctx.occupants;
            process_presence_x ctx xmpp stanza from enter;
            if enter then
              ctx.room_widget#muc_event_join from.resource;
            if stanza.content.presence_type = Some Unavailable then
              if from.lresource = ctx.mynick then
                Grgn_xmpp.remove_hook xmpp from
              else
                ctx.occupants <-
                  Occupant.remove from.lresource
                  ctx.occupants;
            (* do_hook xmpp stanza hooks*)
            ()
        | _ ->
          (* do_hook xmpp stanza hooks *)
          ()
                    
                          
(* menu callback for "Join Conference" *)
let join () =
  let room = JID.of_string "devel@conference.jabber.ru" in
  let room_widget = new muc_room room ~packing:(fun w -> appwin#add_chat w) () in
  let mynick = "gorgona" in
    Grgn_xmpp.add_hook appwin#xmpp room
      (process_presence {
        room_widget;
        mynick;
        occupants = Occupant.empty
      });
    XEP_muc.enter_room appwin#xmpp ~nick:"gorgona" room
