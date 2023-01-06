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
  last_pos : Raylib.Vector2.t;
  disp : Raylib.Vector2.t;
  accel : Raylib.Vector2.t;
  radius : float;
  friction_c : float;
  friction_vec : Raylib.Vector2.t;
  color : color;
}

let substeps = 2
let color b = b.color
let pos b = b.pos
let vel b dt = b.pos <-> b.last_pos <*> 1. /. dt
let radius b = b.radius
let touching b1 b2 = distance (pos b1) (pos b2) < radius b1 +. radius b2
let moving b = vec_pair b.disp = (0., 0.) && vec_pair b.accel = (0., 0.)
let set_accel b accel = { b with accel }
let set_pos b pos = { b with pos }

(** calculates displacement*)
let update_disp b =
  let d = subtract b.pos b.last_pos in
  if length d > 0.1 then { b with disp = d }
  else { b with disp = zero () }

(** sets friction*)
let friction b =
  if vec_equal b.disp (zero ()) then zero ()
  else normalize b.disp <*> ~-.(b.friction_c)

let update b dt =
  let f = b.accel in
  let b = b |> update_disp in
  let new_pos = b.disp <+> (f <*> dt ** 2.) <+> b.pos in
  if distance b.last_pos new_pos > distance b.last_pos b.pos then
    {
      b with
      pos = new_pos;
      last_pos = b.pos;
      friction_vec = friction b;
    }
  else { b with pos = b.last_pos; friction_vec = zero () }

let tick dt b =
  let n = substeps in
  let i, rb = (ref n, ref b) in
  while !i > 0 do
    rb := update !rb (dt /. float_of_int n);
    i := !i - 1
  done;
  set_accel !rb (zero ())

let init (x, y) r f c =
  {
    pos = create x y;
    last_pos = create x y;
    accel = zero ();
    disp = zero ();
    radius = r;
    friction_c = f;
    friction_vec = zero ();
    color = c;
  }
