open Raylib

type t = {
  dist : float;
  angle : float;
  vel : float;
  clicked : bool;
  released : bool;
  power : float;
  length : float;
  target : Vector2.t;
  clicked_pos : Vector2.t;
  contact : bool;
}

let target c = c.target
let dist c = c.dist
let angle c = c.angle
let length c = c.length
let vel c = c.vel
let contact c = c.contact
let power c = c.power

let init (x, y) =
  {
    dist = 0.;
    angle = 0.;
    vel = 0.;
    released = false;
    clicked = false;
    length = 300.;
    power = 0.;
    target = Vector2.create x y;
    clicked_pos = Vector2.zero ();
    contact = false;
  }

(*NOTE: angles are defined such that an increase in angle is CLOCKWISE,
  this is because y values increase downwards*)
let vec_angle v1 v2 =
  let open Vector2 in
  let v = subtract v1 v2 in
  let x, y = (x v, y v) in
  atan2 y x

(** helper for dragging cue controls*)
let held_distance c mouse_pos =
  Vector2.(
    let a = vec_angle mouse_pos c.target -. c.angle in
    let d =
      (Float.cos a *. distance mouse_pos c.target)
      -. distance c.clicked_pos c.target
    in
    if d < 0. then 0. else if d > 200. then 200. else d)

(** cue is being dragged back*)
let dragging_back c mouse_pos =
  {
    c with
    clicked_pos = (if c.clicked then c.clicked_pos else mouse_pos);
    clicked = true;
    dist = held_distance c mouse_pos;
  }

(** cue has struck cue ball*)
let contacted c =
  {
    c with
    released = false;
    clicked = false;
    vel = 0.;
    dist = 0.;
    contact = true;
  }

(** cue has just been released*)
let released c =
  {
    c with
    released = true;
    vel = (c.dist +. 5.) /. 3.;
    power = c.dist /. 5.;
  }

(** cue is following mouse*)
let moving c mouse_pos target =
  {
    c with
    angle = vec_angle mouse_pos c.target;
    clicked = false;
    power = 0.;
    contact = false;
    target;
  }

let tick c mouse_pos mouse_down no_movement target =
  if mouse_down then dragging_back c mouse_pos
  else if c.clicked then
    if c.dist < 0. then contacted c
    else if c.released then { c with dist = c.dist -. c.vel }
    else released c
  else if no_movement then moving c mouse_pos target
  else { c with contact = false }
