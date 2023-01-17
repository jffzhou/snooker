open Raylib.Vector2

type mouse_action =
  | None of t
  | Click of t
  | Dragging of t * t
  | Released of mouse_action

type t

val init : unit -> t
val action : t -> mouse_action
val tick : t -> t