(* open Effect *)

type 'a resumer = 'a -> unit
type _ Effect.t += Stuck : unit Effect.t
type _ Effect.t += Suspend : (('a resumer -> bool) ) -> 'a Effect.t