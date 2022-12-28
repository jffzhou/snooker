(** representation of a ball*)

type t
(** abstract type representing a ball*)

type color =
  | Red
  | Yellow
  | Green
  | Brown
  | Blue
  | Pink
  | Black
  | Cue

val pos : t -> Vector2d.t
val vel : t -> Vector2d.t
val radius : t -> float
val color : t -> color

val touching : t -> t -> bool
(** [touching b1 b2] is [true] if [b1] and [b2] are touching and [false]
    otherwise.*)

val tick : t -> t

val init : float * float -> float -> float -> color -> t
(** [init (x, y) r f c] initialises a new ball with position [p], radius
    [r], friction [f] and color string [c].*)
