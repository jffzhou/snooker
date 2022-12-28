open Vector2d

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
  pos : Vector2d.t;
  vel : Vector2d.t;
  radius : float;
  friction : float;
  color : color;
}

let color b = b.color
let pos b = b.pos
let radius b = b.radius
let vel b = b.vel
let touching b1 b2 = dist (pos b1) (pos b2) < radius b1 +. radius b2

let tick b =
  { b with pos = add b.pos b.vel; vel = mult b.vel b.friction }

let init (x, y) r f c =
  { pos = vec x y; vel = zero; radius = r; friction = f; color = c }
