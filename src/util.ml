open Float
open Raylib.Vector2

let multiply_scalar v1 s = multiply v1 (create s s)
let vec_pair v = (x v, y v)
let ( <=> ) v1 v2 = x v1 = x v2 && y v1 = y v2
let vec_map f v1 = create (v1 |> x |> f) (v1 |> y |> f)
let ( <+> ) v1 v2 = add v1 v2
let ( <*> ) v1 s = multiply_scalar v1 s
let ( <-> ) v1 v2 = subtract v1 v2

let point_line_dist p1 p2 p0 =
  let x1, y1 = vec_pair p1 in
  let x2, y2 = vec_pair p2 in
  let x0, y0 = vec_pair p0 in
  abs (((x2 -. x1) *. (y1 -. y0)) -. ((x1 -. x0) *. (y1 -. y0)))
  /. distance p1 p2

let reflect_x v =
  let x, y = vec_pair v in
  create x ~-.y

let reflect_y v =
  let x, y = vec_pair v in
  create ~-.x y

let x_hat () = create 1. 0.
let y_hat () = create 0. 1.
let create_pair (x, y) = create x y
let string_of_vec v = Printf.sprintf "(%f, %f)" (x v) (y v)

let is_obtuse s1 s2 s3 =
  let m = List.fold_left max min_float [ s1; s2; s3 ] in
  if m = s1 then s1 ** 2. > (s2 ** 2.) +. (s3 ** 2.)
  else if m = s2 then s2 ** 2. > (s1 ** 2.) +. (s3 ** 2.)
  else s3 ** 2. > (s2 ** 2.) +. (s1 ** 2.)
