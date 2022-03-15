(* Taken from https://github.com/kayceesrk/code-snippets/blob/master/scheduler_parateric_mvar/Sched.mli *)

(* open Effect *)

type 'a resumer = 'a -> unit
type _ Effect.t += Stuck : unit Effect.t
type _ Effect.t += Suspend : (('a resumer -> bool) ) -> 'a Effect.t