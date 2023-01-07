open Ball
open Raylib.Vector2
open Cue
open Raylib
open LineBoundary

let substeps = 16

type t = {
  balls : Ball.t list;
  cue : Cue.t;
  line_boundaries : LineBoundary.t list;
}

let ball_radius = 10.
let cueball t = List.filter (fun x -> color x = Cue) t.balls |> List.hd
let cueball_pos t = cueball t |> pos
let init_balls = [ Ball.init (400., 400.) ball_radius 0.97 Cue ]

let init_line_boundaries =
  let tl, tr, bl, br =
    ( create 200. 200.,
      create 600. 200.,
      create 200. 600.,
      create 600. 600. )
  in
  LineBoundary.[ init tl tr; init tl bl; init tr br; init bl br ]

let init =
  {
    balls = init_balls;
    cue = Cue.init (400., 400.);
    line_boundaries = init_line_boundaries;
  }

let balls t = t.balls
let cue t = t.cue
let line_boundaries t = t.line_boundaries

let hit_cueball b c dt =
  let a = Float.pi +. angle c in
  let v =
    Float.(
      create
        ((a |> cos) *. power c /. dt)
        ((a |> sin) *. (power c /. dt)))
  in
  List.map (fun x -> if color x = Cue then set_accel x v else x) b

(** applies boundary conditions for a singular ball*)
let rec apply_boundary_ball b bl =
  match bl with
  | [] -> b
  | h :: t ->
      if collision h b then update_vel h b else apply_boundary_ball b t

let apply_boundary bl =
  let rec apply_boundary_rec acc = function
    | [] -> acc
    | h :: t -> apply_boundary_rec (apply_boundary_ball h bl :: acc) t
  in
  apply_boundary_rec []

let update_balls balls line_boundaries =
  let i, rb = (ref substeps, ref balls) in
  while !i > 0 do
    rb := apply_boundary line_boundaries !rb;
    rb := List.map (Ball.tick (1. /. float_of_int substeps)) !rb;
    i := !i - 1
  done;
  !rb

let tick t =
  let cue =
    Cue.tick t.cue (get_mouse_position ())
      (is_mouse_button_down MouseButton.Left)
      (Ball.moving (cueball t))
      (cueball_pos t)
  in
  let t =
    if Cue.contact cue then
      {
        t with
        balls = hit_cueball t.balls t.cue (1. /. float_of_int substeps);
      }
    else t
  in
  { t with balls = update_balls t.balls t.line_boundaries; cue }
