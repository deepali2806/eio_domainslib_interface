(* Taken from https://github.com/kayceesrk/code-snippets/blob/master/scheduler_parateric_mvar/Sched.mli *)

open EffectHandlers

type 'a resumer = 'a -> unit
type _ eff += Stuck : unit eff
type _ eff += Suspend : ('a resumer -> unit) -> 'a eff
