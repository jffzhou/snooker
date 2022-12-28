open Snooker
open Raylib
open Vector2d

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
    let x, y = b |> pos |> vec_pair in
    draw_circle (int_of_float x) (int_of_float y) (radius b)
      (b |> color |> color_to_raylib))

let rec draw_balls b =
  match b with
  | [] -> ()
  | h :: t ->
      draw_ball h;
      draw_balls t

let draw t =
  begin_drawing ();
  clear_background Color.darkgreen;
  t |> balls |> draw_balls;
  end_drawing ()
