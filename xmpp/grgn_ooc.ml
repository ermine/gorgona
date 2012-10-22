(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open Xml
open JID

open Grgn_xmpp
open XMPPClient

class type ooc_window =
object
  method mynick : string
  method set_mynick : string -> unit
    
  method display_message : string -> string -> unit
end

let process_message self xmpp stanza =
  match stanza.jid_from with
    | None -> () (* TODO *)
    | Some from ->        
      match stanza.content.body with
        | None -> () (* TODO *)
        | Some body -> self#display_message from.resource body
            
