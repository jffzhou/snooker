open Raylib.Vector2
open Util

type color =
  | Red
  | Yellow
  | Green
  | Brown
  | Blue
  | Pink
  | Black
  | Cue of int

type t = {
  pos : Raylib.Vector2.t;
  vel : Raylib.Vector2.t;
  accel : Raylib.Vector2.t;
  radius : float;
  friction_c : float;
  color : color;
  colliding : bool;
}

let color b = b.color
let pos b = b.pos
let vel b = b.vel
let radius b = b.radius
let touching b1 b2 = distance (pos b1) (pos b2) < radius b1 +. radius b2
let moving b = not (b.vel <=> zero ())
let set_accel accel b = { b with accel }
let set_pos pos b = { b with pos }
let set_vel vel b = { b with vel }

let is_cue b player =
  match b.color with
  | Cue p -> if p = 0 then true else p = player
  | _ -> false

let set_colliding colliding b = { b with colliding }

let tick dt b =
  let f = b.accel in
  let new_v =
    (b.vel <+> (f <*> dt)
    <*> if b.colliding then 1. else b.friction_c ** dt)
    |> fun x -> if length x < 0.5 && not b.colliding then zero () else x
  in
  {
    b with
    vel = new_v;
    pos = b.pos <+> (new_v <*> dt);
    accel = zero ();
    colliding = false;
  }

let init (x, y) r f c =
  {
    pos = create x y;
    vel = zero ();
    accel = zero ();
    radius = r;
    friction_c = f;
    color = c;
    colliding = false;
  }

let resolve_collision_elastic b1 b2 =
  let x1, x2, v1, v2 = (b1.pos, b2.pos, b1.vel, b2.vel) in
  let one_collision v1 v2 x1 x2 =
    v1
    <-> (x1 <-> x2
        <*> dot_product (v1 <-> v2) (x1 <-> x2) /. length_sqr (x1 <-> x2)
        )
  in
  let v1_n = one_collision v1 v2 x1 x2 in
  let v2_n = one_collision v2 v1 x2 x1 in
  let disp_vec =
    normalize (x1 <-> x2)
    <*> b1.radius +. b2.radius -. length (x1 <-> x2)
  in
  let x1_n = x1 <+> (disp_vec <*> 0.5) in
  let x2_n = x2 <-> (disp_vec <*> 0.5) in
  ( { b1 with vel = v1_n; pos = x1_n; colliding = true },
    { b2 with vel = v2_n; pos = x2_n; colliding = true } )
