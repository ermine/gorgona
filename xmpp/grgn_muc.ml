(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open Xml
open JID

open Grgn_xmpp
module MUC = XEP_muc.Make (XMPPClient)
open XMPPClient
open MUC

type occupant = {
  (* nick : string; *)
  mutable jid : JID.t option;
  mutable affiliation : affiliation;
  mutable role : role
}

class type groupchat_window =
object
  method mynick : string
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

  method muc_event_decline : JID.t option -> JID.t option ->
    string option -> unit -> unit

  method muc_event_invite : JID.t option -> JID.t option ->
    string option -> string option -> unit -> unit

  method display_message : string -> string -> unit
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
                  if from.lresource = self#mynick then
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
              if from.lresource = self#mynick then
                Grgn_xmpp.remove_page_window xmpp (Grgn_xmpp.lpair from)
              else
                self#remove_occupant from.lresource;
            (* do_hook xmpp stanza hooks*)
            ()
        | _ ->
          (* do_hook xmpp stanza hooks *)
          ()

let process_message_status self xmpp stanza status =
  List.iter 
    (function
      | 100
       (* context: Entering a room *)
       (* Inform user that any occupant is allowed to see the user's
          full JID *)
      | 101
       (* message out of band *)
       (* context Affiliation change *)
       (* Inform user that his or her affiliation changed
          while not in the room *)
      | 102
       (* context Configuration change *)
       (* Inform occupants that room now shows unavailable
          members *)
      | 103
       (* context Configuration change *)
       (* Inform occupants that room now does not show
          unavailable members *)
      | 104
       (* context Configuration change *)
       (* Inform occupants that a non-privacy-related room
          configuration change has occured *)
      | 170
       (* context Configuration change *)
       (* Inform occupants that room logging is now enabled *)
      | 171
       (* context Configuration change *)
       (* Inform occupants that room logging is now disabled *)
      | 172
       (* context Configuration change *)
       (* Inform occupants that the room is now non- anonymous *)
      | 173
       (* context Configuration change *)
       (* Inform occupants that the room is now semi- anonymous *)
      | 174 ->
           (* context Configuration change *)
           (* Inform occupants that the room is now fully-anonymous *)
        ()
      | _ ->
        ()
    ) status
    
let process_message_user_x self xmpp stanza from data =
  let () =
    match stanza.content.message_type with
      | None
      | Some Normal -> (
        match data.User.decline with
          | None -> ()
          | Some (jid_from, jid_to, reason) ->
            self#muc_event_decline jid_from jid_to reason ()
      );
        List.iter
          (fun (jid_from, jid_to, reason) ->
            self#muc_event_invite jid_from jid_to reason data.User.password ()
          ) data.User.invite
      | Some Groupchat ->
        ()
      | _ ->
        ()
  in
    process_message_status self xmpp stanza data.User.status;
    if data.User.decline <> None || data.User.invite <> [] ||
      data.User.status <> [] then
      true
    else
      false
            
let process_message self xmpp stanza =
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some from ->        
      let continue =
        match opt_try (get_element (ns_muc_user, "x")) stanza.x with
          | Some el ->
            process_message_user_x self xmpp stanza from (User.decode el)
          | None ->
            true
      in
        if continue then
          match stanza.content.body with
            | None -> () (* TODO *)
            | Some body ->
              self#display_message from.resource body
            
