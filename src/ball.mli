(** representation of a ball*)

type t
(** abstract type representing a ball*)

val pos : t -> Vector2d.t
val vel : t -> Vector2d.t

val touching : t -> t -> bool
(** [touching b1 b2] is [true] if [b1] and [b2] are touching and [false]
    otherwise.*)

val tick : t -> t

val init : Vector2d.t -> float -> float -> string -> t
(** [init p r f c] initialises a new ball with position [p], radius [r],
    friction [f] and color string [c].*)
