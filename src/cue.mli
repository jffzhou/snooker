type t

val init : float * float -> float -> t
val tick : t -> Control.mouse_action -> Raylib.Vector2.t option -> t
val angle : t -> float
val dist : t -> float
val vel : t -> float
val length : t -> float
val power : t -> float
val contact : t -> bool
val target : t -> Raylib.Vector2.t