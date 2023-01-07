open Raylib
open Raylib.Vector2
open Float
open Util

type line_type =
  | Vertical
  | Horizontal
  | General

type t = {
  line_type : line_type;
  p1 : Vector2.t;
  p2 : Vector2.t;
}

let init p1 p2 =
  let x1, y1 = vec_pair p1 in
  let x2, y2 = vec_pair p2 in
  if x1 = x2 then { line_type = Vertical; p1; p2 }
  else if y1 = y2 then { line_type = Horizontal; p1; p2 }
  else { line_type = General; p1; p2 }

let points = function
  | { line_type; p1; p2 } -> (p1, p2)

let line_dist b =
  let p = Ball.pos b in
  function
  | { line_type; p1; p2 } -> (
      match line_type with
      | Vertical -> abs (Vector2.x p1 -. Vector2.x p)
      | Horizontal -> abs (Vector2.y p1 -. Vector2.y p)
      | General -> point_line_dist p1 p2 p)

let collision l b =
  let r = Ball.radius b in
  r >= line_dist b l

let update_collision l b =
  let open Ball in
  let v, d, p = (vel b, radius b -. line_dist b l, pos b) in
  match l with
  | { line_type; p1; p2 } -> (
      match line_type with
      | Vertical ->
          b
          |> set_vel (reflect_y v)
          |> set_colliding true
          |> set_pos (p <+> create (if x p > x p1 then d else ~-.d) 0.)
      | Horizontal ->
          b
          |> set_vel (reflect_x v)
          |> set_colliding true
          |> set_pos (p <+> create 0. (if y p > y p1 then d else ~-.d))
      | General -> b (*TODO*))
