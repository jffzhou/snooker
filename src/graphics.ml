open Snooker
open Raylib
open Raylib.Vector2
open LineBoundary
open Init

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
    | Cue _ -> Color.white)

let draw_ball b =
  let open Ball in
  let x, y = b |> pos |> fun v -> (x v, y v) in
  match color b with
  | Cue i ->
      draw_circle (int_of_float x) (int_of_float y) (radius b)
        (if i = 1 then Color.red else Color.blue);
      draw_circle (int_of_float x) (int_of_float y)
        (0.85 *. radius b)
        Color.white
  | c ->
      draw_circle (int_of_float x) (int_of_float y) (radius b)
        (c |> color_to_raylib)

let draw_cue c =
  let rect =
    Rectangle.create
      (Cue.target c |> x)
      (Cue.target c |> y)
      (Cue.length c) 10.
  in
  draw_rectangle_pro rect
    (create ~-.(Cue.dist c +. (ball_radius *. 2.)) 5.)
    (Cue.angle c *. 180. /. Float.pi)
    Color.brown

let draw_line l =
  let p1, p2 = points l in
  draw_line_v p1 p2 Color.white

let rec draw_list f = function
  | [] -> ()
  | h :: t ->
      f h;
      draw_list f t

let debug t = ()

let draw t =
  begin_drawing ();
  clear_background Color.black;
  t |> balls |> draw_list draw_ball;
  t |> line_boundaries |> draw_list draw_line;
  if cueball t <> None then t |> cue |> draw_cue;
  t |> debug;
  end_drawing ()
