type t

val init : float * float -> t

val tick :
  t -> Raylib.Vector2.t -> bool -> bool -> Raylib.Vector2.t -> t

val angle : t -> float
val dist : t -> float
val vel : t -> float
val length : t -> float
val power : t -> float
val contact : t -> bool
val target : t -> Raylib.Vector2.t