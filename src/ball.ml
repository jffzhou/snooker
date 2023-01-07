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
  | Cue

type t = {
  pos : Raylib.Vector2.t;
  vel : Raylib.Vector2.t;
  accel : Raylib.Vector2.t;
  radius : float;
  friction_c : float;
  color : color;
}

let color b = b.color
let pos b = b.pos
let vel b = b.vel
let radius b = b.radius
let touching b1 b2 = distance (pos b1) (pos b2) < radius b1 +. radius b2
let moving b = b.vel <=> zero ()
let set_accel b accel = { b with accel }
let set_pos b pos = { b with pos }
let set_vel b vel = { b with vel }

let tick dt b =
  let f = b.accel in
  let new_v =
    b.vel <+> (f <*> dt) <*> b.friction_c ** dt
    |> vec_map (fun x -> if Float.abs x < 0.05 then 0. else x)
  in
  {
    b with
    vel = new_v;
    pos = b.pos <+> (new_v <*> dt);
    accel = zero ();
  }

let init (x, y) r f c =
  {
    pos = create x y;
    vel = zero ();
    accel = zero ();
    radius = r;
    friction_c = f;
    color = c;
  }

let resolve_collision_elastic b1 b2 =
  let x1, x2, v1, v2 = (b1.pos, b2.pos, b1.vel, b2.vel) in
  let one_collision v1 v2 x1 x2 =
    v1 <-> (x1 <-> x2)
    <*> dot_product (v1 <-> v2) (x1 <-> x2) /. length_sqr (x1 <-> x2)
  in
  let v1_n = one_collision x1 x2 v1 v2 in
  let v2_n = one_collision v2 v1 x2 x1 in
  ({ b1 with vel = v1_n }, { b2 with vel = v2_n })