open Raylib

type t

val init : Vector2.t -> float -> float -> t
val falls : Ball.t -> t -> bool
val pos : t -> Vector2.t
val radius : t -> float