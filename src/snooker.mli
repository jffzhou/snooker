type t

val ball_radius : float
val cueball_pos : t -> Raylib.Vector2.t
val init : t
val tick : t -> t
val balls : t -> Ball.t list
val cue : t -> Cue.t