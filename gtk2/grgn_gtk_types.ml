(*
 * (c) 2005-2012 Anastasia Gornostaeva
 *)

open GObj
open Grgn_xmpp
open XMPPClient

class type page_window =
object
  (* method process_iq : xmpp -> XMPP.iq_stanza -> unit *)
  method process_presence : xmpp -> XMPPClient.presence_stanza -> unit
  method process_message : xmpp -> XMPPClient.message_stanza -> unit
end

class type appwin =
object
  method send_message : ?id:string ->
    ?jid_from:JID.t ->
      ?jid_to:JID.t ->
        ?kind:message_type ->
          ?lang:Xml.cdata ->
            ?body:Xml.cdata ->
              ?subject:Xml.cdata ->
                ?thread:Xml.cdata ->
                  ?x:Xml.element list ->
                    unit -> unit
  
  method add_page : widget -> unit
  method add_page_window : (string * string) -> page_window -> unit
  method process_message : xmpp -> message_content stanza -> unit
  method process_presence : xmpp -> presence_content stanza -> unit
  method process_message_error : xmpp -> ?id:string ->
    ?jid_from:JID.t -> ?jid_to:string -> ?lang:string -> StanzaError.t -> unit
  method process_presence_error : xmpp -> ?id:string ->
    ?jid_from:JID.t -> ?jid_to:string -> ?lang:string -> StanzaError.t -> unit

end

