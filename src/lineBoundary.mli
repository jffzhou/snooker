open Raylib

type t

val init : Vector2.t -> Vector2.t -> t
val collision : t -> Ball.t -> bool
val set_touching : t -> Ball.t -> Ball.t
val points : t -> Vector2.t * Vector2.t
