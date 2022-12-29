open Ball
open Raylib.Vector2
open Cue
open Raylib

type t = {
  balls : Ball.t list;
  cue : Cue.t;
}

let ball_radius = 10.
let cueball t = List.filter (fun x -> color x = Cue) t.balls |> List.hd
let cueball_pos t = cueball t |> pos
let cueball_vel t = cueball t |> Ball.vel
let init_balls = [ Ball.init (400., 400.) ball_radius 0.95 Cue ]
let init = { balls = init_balls; cue = Cue.init (400., 400.) }
let balls t = t.balls
let cue t = t.cue

let hit_cueball b c =
  let a = Float.pi +. angle c in
  let v =
    Float.(create ((a |> cos) *. power c) ((a |> sin) *. power c))
  in
  List.map (fun x -> if color x = Cue then set_vel x v else x) b

let tick t =
  let cue =
    Cue.tick t.cue (get_mouse_position ())
      (is_mouse_button_down MouseButton.Left)
      (cueball_vel t |> Vector2.length < 0.2)
      (cueball_pos t)
  in
  let t =
    if Cue.contact cue then { t with balls = hit_cueball t.balls t.cue }
    else t
  in
  { balls = List.map Ball.tick t.balls; cue }
