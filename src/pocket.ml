open Raylib

type t = Line.t

let init = Line.init
let points = Line.points
let is_goal b p = (Line.check_collision b false p).collision
