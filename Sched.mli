open EffectHandlers

type 'a resumer = 'a -> unit
type _ eff += Stuck : unit eff
type _ eff += Suspend : (('a resumer -> bool) ) -> 'a eff