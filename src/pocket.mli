open Raylib

type t

val init : Vector2.t -> Vector2.t -> t
val points : t -> Vector2.t * Vector2.t
val is_goal : Ball.t -> t -> bool