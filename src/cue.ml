open Raylib.Vector2

type t = {
  pos : Raylib.Vector2.t;
  vel : Raylib.Vector2.t;
  clicked : bool;
  released : bool;
  power : float;
  target : Raylib.Vector2.t;
}

let pos c = c.pos
let vel c = c.pos

let init (x, y) =
  {
    pos = create x y;
    vel = zero ();
    released = false;
    clicked = false;
    power = 0.;
    target = create x y;
  }

let tick c = c
