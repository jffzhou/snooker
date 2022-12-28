open Ball
open Raylib.Vector2
open Cue
open Raylib

type t = {
  balls : Ball.t list;
  cue : Cue.t;
}

let init_balls = [ Ball.init (400., 400.) 10. 0.9 Cue ]
let init = { balls = init_balls; cue = Cue.init (0., 0.) }
let balls t = t.balls
let cue t = t.cue

let tick t =
  { balls = List.map Ball.tick t.balls; cue = Cue.tick t.cue }
