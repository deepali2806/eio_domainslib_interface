(* open Effect *)

type 'a resumer = ('a, exn) result -> bool
type _ Effect.t += Stuck : unit Effect.t
type _ Effect.t += Suspend : ('a resumer -> bool) -> 'a Effect.t
