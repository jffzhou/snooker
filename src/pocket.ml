open Raylib

type t = {
  pos : Vector2.t;
  radius : float;
  max_speed : float;
}

let init pos radius max_speed = { pos; radius; max_speed }
let pos p = p.pos
let radius p = p.radius

let falls b p =
  let open Ball in
  let open Vector2 in
  let ball_p, ball_s = (pos b, vel b |> length) in
  ball_s < p.max_speed && distance ball_p p.pos < p.radius
