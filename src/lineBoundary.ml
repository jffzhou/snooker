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

let resolve_corner_collision b p v corner_p depth =
  let open Ball in
  let norm = normalize (p <-> corner_p) in
  let new_v = v <-> (norm <*> 2. *. dot_product v norm) in
  b |> set_vel new_v |> set_colliding true
  |> set_pos (p <+> (norm <*> depth))

let check_corner_collision b p v r p1 p2 =
  let dist = distance p1 p in
  if dist < r then Some (resolve_corner_collision b p v p1 (r -. dist))
  else
    let dist = distance p2 p in
    if dist < r then
      Some (resolve_corner_collision b p v p2 (r -. dist))
    else None

let resolve_vertical_collision b p v boundary_x depth =
  let open Ball in
  b
  |> set_vel (reflect_y v)
  |> set_colliding true
  |> set_pos
       (p <+> create (if x p > boundary_x then depth else ~-.depth) 0.)

let resolve_horizontal_collision b p v boundary_y depth =
  let open Ball in
  b
  |> set_vel (reflect_x v)
  |> set_colliding true
  |> set_pos
       (p <+> create 0. (if y p > boundary_y then depth else ~-.depth))

let check_vertical_collision b p1 p2 =
  let r, ball_p, v = Ball.(radius b, pos b, vel b) in
  let ball_x, ball_y = vec_pair ball_p in
  if (ball_y -. y p1) *. (ball_y -. y p2) < 0. then
    let dist = abs (x p1 -. ball_x) in
    if r >= dist then
      Some (resolve_vertical_collision b ball_p v (x p1) (r -. dist))
    else None
  else check_corner_collision b ball_p v r p1 p2

let check_horizontal_collision b p1 p2 =
  let r, ball_p, v = Ball.(radius b, pos b, vel b) in
  let ball_x, ball_y = vec_pair ball_p in
  if (ball_x -. x p1) *. (ball_x -. x p2) < 0. then
    let dist = abs (y p1 -. ball_y) in
    if r >= dist then
      Some (resolve_horizontal_collision b ball_p v (y p1) (r -. dist))
    else None
  else check_corner_collision b ball_p v r p1 p2

let resolve_boundary_collision l b =
  match l with
  | { line_type; p1; p2 } -> (
      match line_type with
      | Vertical -> check_vertical_collision b p1 p2
      | Horizontal -> check_horizontal_collision b p1 p2
      | General -> None (*TODo*))
