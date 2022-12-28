open Vector2d

type t = {
  pos : Vector2d.t;
  vel : Vector2d.t;
  released : bool;
  power : float;
  target : Vector2d.t;
}

let pos c = c.pos
let vel c = c.pos

let init (x, y) =
  {
    pos = vec x y;
    vel = zero;
    released = false;
    power = 0.;
    target = vec x y;
  }

let tick c = c
