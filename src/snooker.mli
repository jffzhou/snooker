type t

val init : t
val tick : t -> t
val balls : t -> Ball.t list
val cue : t -> Cue.t