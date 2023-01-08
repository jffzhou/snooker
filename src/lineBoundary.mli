open Raylib

type t

val init : Vector2.t -> Vector2.t -> t
val resolve_boundary_collision : t -> Ball.t -> Ball.t option
val points : t -> Vector2.t * Vector2.t
