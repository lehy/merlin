type item_desc =
  | Root
  | Definition of Parsetree.structure_item * item_desc
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * item_desc

type item = Outline.Chunked.sync * item_desc
type sync = item History.sync
type t = item History.t

exception Malformed_module
exception Invalid_chunk

val empty : item_desc
val sync_step : Outline_utils.kind -> Outline.Raw.item list -> item_desc -> item_desc

val sync : Outline.Chunked.t -> t -> t