open Snooker
open Raylib
open Raylib.Vector2
open LineBoundary
open Init
open Util

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
  let sunk = is_sunk b in
  match color b with
  | Cue i ->
      draw_circle (int_of_float x) (int_of_float y) (radius b)
        (if i = 1 then Color.red else Color.blue);
      draw_circle (int_of_float x) (int_of_float y)
        (0.85 *. radius b)
        (if sunk then Color.create 255 255 255 100 else Color.white)
  | c ->
      draw_circle (int_of_float x) (int_of_float y) (radius b)
        (if sunk then Color.create 255 255 255 40
        else c |> color_to_raylib)

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

let c p1 p2 start_pos end_pos x =
  check_collision_lines p1 p2 start_pos end_pos
    (Ctypes.allocate Vector2.t start_pos)

let debug t =
  draw_text (Printf.sprintf "%i" (get_fps ())) 200 200 30 Color.white

let find_end_pos start_pos t =
  let rec helper end_pos = function
    | [] -> end_pos
    | h :: t ->
        let open Ctypes in
        let p1, p2 = points h in
        let v_ptr = allocate Vector2.t (zero ()) in
        if check_collision_lines p1 p2 start_pos end_pos v_ptr then
          helper !@v_ptr t
        else helper end_pos t
  in
  let angle = (t |> cue |> Cue.angle) +. Float.pi in
  helper (start_pos <+> (create (cos angle) (sin angle) <*> 1000.))

let draw_indicator t =
  let start_pos = t |> cue |> Cue.target in
  let end_pos = find_end_pos start_pos t (t |> line_boundaries) in
  draw_line_ex start_pos end_pos 2. Color.gray

let draw t =
  begin_drawing ();
  clear_background Color.black;
  if cueball t <> None then draw_indicator t;
  t |> balls |> draw_list draw_ball;
  t |> line_boundaries |> draw_list draw_line;
  if cueball t <> None then t |> cue |> draw_cue;
  t |> debug;
  end_drawing ()
