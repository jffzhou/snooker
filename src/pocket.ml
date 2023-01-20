open Raylib

type t = LineBoundary.t

let init = LineBoundary.init
let points = LineBoundary.points
let is_goal b p = LineBoundary.resolve_boundary_collision p b <> None
