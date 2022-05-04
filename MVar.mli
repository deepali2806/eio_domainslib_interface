type 'a t
val sw : bool ref
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> (unit, string) result
val take : 'a t -> ('a, string) result