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
  | Cue of int

val pos : t -> Raylib.Vector2.t
val is_cue : t -> int -> bool
val radius : t -> float
val color : t -> color
val moving : t -> bool
val vel : t -> Raylib.Vector2.t
val sink : t -> t
val is_sunk : t -> bool
val reset : t -> t
val sunk_time : t -> float

val touching : t -> t -> bool
(** [touching b1 b2] is [true] if [b1] and [b2] are touching and [false]
    otherwise.*)

val tick : float -> t -> t

val init : float * float -> float -> float -> color -> t
(** [init (x, y) r f c] initialises a new ball with position [p], radius
    [r], friction [f] and color string [c].*)

val set_accel : Raylib.Vector2.t -> t -> t
val set_pos : Raylib.Vector2.t -> t -> t
val set_vel : Raylib.Vector2.t -> t -> t
val resolve_collision_elastic : t -> t -> t * t
val set_colliding : bool -> t -> t