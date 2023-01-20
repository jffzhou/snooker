open Raylib

type line_type =
  | Vertical
  | Horizontal
  | General

type collision_info = {
  collision : bool;
  depth : float;
  corner : bool;
  collision_point : Vector2.t;
}

type t

val init : Vector2.t -> Vector2.t -> t
val points : t -> Vector2.t * Vector2.t
val line_type : t -> line_type
val line_dist : Vector2.t -> t -> float
val check_collision : Ball.t -> bool -> t -> collision_info