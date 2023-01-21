open Raylib

type t

val init : Vector2.t -> Vector2.t -> int -> t
val player : t -> int
val points : t -> Vector2.t * Vector2.t
val is_goal : Ball.t -> t -> bool