(*
 * (c) 2005-20011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj
open GMisc
open Gobject.Data
  
class muc_roster ?packing () =
  let cols = new GTree.column_list in
  let col_item = cols#add string in
  let model = GTree.tree_store cols in
  let view = GTree.view ~model ?packing () in
object (self)
  inherit widget view#as_widget
end

class muc_room ?packing () =
  let vpaned = GPack.paned `VERTICAL ?packing () in
  let hpaned = GPack.paned `HORIZONTAL ~height:200 ~packing:vpaned#add1 () in
  let scrolledchat = GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`AUTOMATIC
    ~placement:`TOP_LEFT ~height:500 ~width:500 ~packing:hpaned#add1 () in
  let _chatlog = GText.view ~editable:false ~wrap_mode:`WORD
    ~packing:scrolledchat#add () in
  let _input = GText.view ~height:50 ~editable:true ~wrap_mode:`WORD
    ~packing:vpaned#add2 () in
  let scrolledroster = GBin.scrolled_window ~hpolicy:`AUTOMATIC
    ~vpolicy:`AUTOMATIC ~placement:`TOP_LEFT ~packing:hpaned#add2 () in
  let _roster = new muc_roster ~packing:scrolledroster#add () in
    
object (self)
  inherit widget vpaned#as_widget
end
