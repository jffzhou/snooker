open Raylib.Vector2
open Line
open Util

type t = Line.t

let points = Line.points
let init = Line.init

let resolve_horizontal_collision b boundary_y depth =
  let open Ball in
  let p, v = (pos b, vel b) in
  b
  |> set_vel (reflect_x v)
  |> set_colliding true
  |> set_pos
       (p <+> create 0. (if y p > boundary_y then depth else ~-.depth))

let resolve_vertical_collision b boundary_x depth =
  let open Ball in
  let p, v = (pos b, vel b) in
  b
  |> set_vel (reflect_y v)
  |> set_colliding true
  |> set_pos
       (p <+> create (if x p > boundary_x then depth else ~-.depth) 0.)

let resolve_corner_collision b corner_p depth =
  let open Ball in
  let p, v = (pos b, vel b) in
  let norm = normalize (p <-> corner_p) in
  let new_v = v <-> (norm <*> 2. *. dot_product v norm) in
  b |> set_vel new_v |> set_colliding true
  |> set_pos (p <+> (norm <*> depth))

let resolve_boundary_collision l b =
  let { collision; depth; corner; collision_point } =
    check_collision b true l
  in
  if collision then
    if corner then
      Some (resolve_corner_collision b collision_point depth)
    else
      match line_type l with
      | Vertical ->
          Some (resolve_vertical_collision b (x collision_point) depth)
      | Horizontal ->
          Some
            (resolve_horizontal_collision b (y collision_point) depth)
      | _ -> failwith "todo"
  else None