type t

val cueball : t -> Ball.t option
val init : t
val tick : t -> t
val balls : t -> Ball.t list
val pockets : t -> Pocket.t list
val cue : t -> Cue.t
val line_boundaries : t -> LineBoundary.t list