open Ball
open Raylib.Vector2
open Cue
open Raylib
open LineBoundary

let substeps = 8

type t = {
  balls : Ball.t list;
  cue : Cue.t;
  line_boundaries : LineBoundary.t list;
}

let ball_radius = 10.
let cueball t = List.filter (fun x -> color x = Cue) t.balls |> List.hd
let cueball_pos t = cueball t |> pos

let random_color () =
  match Random.int 7 with
  | 0 -> Ball.Red
  | 1 -> Ball.Yellow
  | 2 -> Ball.Green
  | 3 -> Ball.Brown
  | 4 -> Ball.Blue
  | 5 -> Ball.Pink
  | 6 -> Ball.Black
  | _ -> failwith "impossible"

let new_ball () =
  Ball.init
    (Random.float 100. +. 350., Random.float 100. +. 200.)
    ball_radius 0.96 (random_color ())

let init_balls =
  let rec random_ball b_list n =
    if n = 0 then b_list
    else
      let new_b = ref (new_ball ()) in
      while
        List.fold_left (fun x y -> x || touching y !new_b) false b_list
      do
        new_b := new_ball ()
      done;
      random_ball (!new_b :: b_list) (n - 1)
  in
  random_ball [ Ball.init (400., 400.) ball_radius 0.96 Cue ] 10

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
  List.map (fun x -> if color x = Cue then set_accel v x else x) b

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

let all_ball_collisions lst =
  let fold_helper x y =
    if touching (fst x) y then
      let b1, b2 = resolve_collision_elastic (fst x) y in
      (b1, snd x @ [ b2 ])
    else (fst x, snd x @ [ y ])
  in
  let rec get_collisions acc = function
    | [] -> List.rev acc
    | h :: t ->
        let i_n, t_n = List.fold_left fold_helper (h, []) t in
        get_collisions (i_n :: acc) t_n
  in
  get_collisions [] lst

let update_balls balls line_boundaries =
  let i, rb = (ref substeps, ref balls) in
  while !i > 0 do
    rb := apply_boundary line_boundaries !rb;
    rb := all_ball_collisions !rb;
    rb := List.map (Ball.tick (1. /. float_of_int substeps)) !rb;
    i := !i - 1
  done;
  !rb

let tick t =
  let cue =
    Cue.tick t.cue (get_mouse_position ())
      (is_mouse_button_down MouseButton.Left)
      (List.for_all Ball.moving t.balls)
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
