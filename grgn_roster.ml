(*
 * (c) 2005-2011 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open GObj
open Gobject.Data
  
(*
 * account
 *  group
 *   subgroup
 *    item
 *     resource
 *      personal info
 *)

(*
let custom_list = new GTree.custom_list
let col_group = column_list#add 
*)

(*
class custom_tree_view =
object (self)
  inherit [custom_tree, custom_tree, unit, unit] GTree.custom_tree_model column_list
*)  

class roster ?packing () =
  let cols = new GTree.column_list in
  let col_item = cols#add string in
  let model = GTree.tree_store cols in
  let view = GTree.view ~model ?packing () in
object (self)
  inherit widget view#as_widget
end
