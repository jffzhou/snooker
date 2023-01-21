open Raylib

type t = {
  line : Line.t;
  player : int;
}

let init p1 p2 player = { line = Line.init p1 p2; player }
let points t = Line.points t.line
let is_goal b p = (Line.check_collision b false p.line).collision
let player t = t.player