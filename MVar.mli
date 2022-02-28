(* Taken from https://github.com/kayceesrk/code-snippets/blob/master/scheduler_parateric_mvar/MVar.mli *)
type 'a t
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> unit
val take : 'a t -> 'a
val check : unit -> unit
