open Vector2d

type t = {
  pos : Vector2d.t;
  vel : Vector2d.t;
  radius : float;
  friction : float;
  color : string;
}

let pos b = b.pos
let vel b = b.vel
let touching b1 b2 = dist b1.pos b2.pos < b1.radius +. b2.radius

let tick b =
  { b with pos = add b.pos b.vel; vel = mult b.vel b.friction }

let init p r f c =
  { pos = p; vel = zero; radius = r; friction = f; color = c }
