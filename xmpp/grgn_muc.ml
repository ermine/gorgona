(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open XMPP
open JID
open XEP_muc

type occupant = {
  (* nick : string; *)
  mutable jid : JID.t option;
  mutable affiliation : affiliation;
  mutable role : role
}

class type groupchat_window =
object
  method get_mynick : string
  method set_mynick : string -> unit
    
  method add_occupant : string -> occupant -> unit
  method find_occupant : string -> occupant
  method remove_occupant : string -> unit
  method mem_occupant : string -> bool
    
  method muc_event_join : string -> unit

  method muc_event_leave : string -> string option -> unit

  method muc_event_nick_changed : string -> string -> string option -> unit

  method muc_event_destroy : JID.t option ->
    string option -> string option  -> unit -> unit
      
  method muc_event_ban : string option -> unit

  method muc_event_kick : string option -> unit

  method muc_event_room_created : unit -> unit

  method muc_event_affiliation : string option -> unit

  method muc_event_members_only : string option -> unit
      
  method muc_event_system_shutdown : string option -> unit

  method muc_event_decline : JID.t -> JID.t -> string option -> unit

  method muc_event_invite : JID.t -> JID.t ->
    string option -> string option -> unit
end
    

type room_ctx = {
  room_window : groupchat_window;
}

let get_reason = function
  | None -> None
  | Some i -> i.User.reason
  

let process_presence_user_x self xmpp stanza  from data enter =
  let () =
    match data.User.item with
      | None -> ()
      | Some item ->
        if stanza.content.presence_type = None then
          let occupant = self#find_occupant from.lresource in
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
          self#muc_event_destroy venue data.User.password reason ();
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
          self#muc_event_room_created ();
        removal
      | 210 ->
            (* context Entering a room *)
            (* Inform user that service has assigned or modified
               occupant's roomnick *)
        if enter then
          self#set_mynick from.lresource;
        removal
      | 301 ->
            (* context Removal from room *)
            (* Inform user that he or she has been banned from the room *)
        if stanza.content.presence_type = Some Unavailable then
          let reason = get_reason data.User.item in
            self#muc_event_ban reason;
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
                  if from.lresource = self#get_mynick then
                    self#set_mynick newnick;
                  self#add_occupant newnick  (self#find_occupant from.lresource);
                  let reason = get_reason data.User.item in
                    self#muc_event_nick_changed
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
            self#muc_event_kick reason;
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
            self#muc_event_affiliation reason;
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
            self#muc_event_members_only reason;
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
            self#muc_event_system_shutdown reason;
            true
        else
          removal
      | _ ->
        removal
    ) removal data.User.status in
    if stanza.content.presence_type = Some Unavailable && not removal then
      self#muc_event_leave from.resource stanza.content.status
        
let process_presence_x self xmpp stanza from enter =
  List.iter (function
    | Xml.Xmlelement (qname, _, _) as el ->
      if qname = (ns_muc_user, "x") then
        process_presence_user_x self xmpp stanza from (User.decode el) enter
    | _ ->
      ()
  ) stanza.x
        
  
let process_presence self xmpp stanza =
  match stanza.jid_from with
    | None -> ()
    | Some from ->
      match stanza.content.presence_type with
        | None
        | Some Unavailable ->
          let enter =
            if stanza.content.presence_type = None &&
              not (self#mem_occupant from.lresource) then
              true
            else
              false
          in
            if enter then
              self#add_occupant from.lresource {jid = None;
                                                affiliation = AffiliationNone;
                                                role = RoleNone};
            process_presence_x self xmpp stanza from enter;
            if enter then
              self#muc_event_join from.resource;
            if stanza.content.presence_type = Some Unavailable then
              if from.lresource = self#get_mynick then
                Grgn_xmpp.remove_page_window xmpp (Grgn_xmpp.lpair from)
              else
                self#remove_occupant from.lresource;
            (* do_hook xmpp stanza hooks*)
            ()
        | _ ->
          (* do_hook xmpp stanza hooks *)
          ()
            
