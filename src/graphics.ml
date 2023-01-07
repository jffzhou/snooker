open Snooker
open Raylib
open Raylib.Vector2
open LineBoundary

let color_to_raylib =
  Ball.(
    function
    | Red -> Color.red
    | Yellow -> Color.yellow
    | Green -> Color.green
    | Brown -> Color.brown
    | Blue -> Color.blue
    | Pink -> Color.pink
    | Black -> Color.black
    | Cue -> Color.white)

let draw_ball b =
  Ball.(
    let x, y = b |> pos |> fun v -> (x v, y v) in
    draw_circle (int_of_float x) (int_of_float y) (radius b)
      (b |> color |> color_to_raylib))

let rec draw_balls = function
  | [] -> ()
  | h :: t ->
      draw_ball h;
      draw_balls t

let draw_cue c =
  let rect =
    Rectangle.create
      (Cue.target c |> x)
      (Cue.target c |> y)
      (Cue.length c) 10.
  in
  draw_rectangle_pro rect
    (create ~-.(Cue.dist c +. ball_radius +. 5.) 5.)
    (Cue.angle c *. 180. /. Float.pi)
    Color.brown

let rec draw_lines (t : Snooker.t) = function
  | [] -> ()
  | h :: tail ->
      let p1, p2 = points h in
      draw_line_v p1 p2 Color.black;
      draw_lines t tail

let debug t = ()

let draw t =
  begin_drawing ();
  clear_background Color.darkgreen;
  t |> balls |> draw_balls;
  t |> cue |> draw_cue;
  t |> line_boundaries |> draw_lines t;
  t |> debug;
  end_drawing ()
