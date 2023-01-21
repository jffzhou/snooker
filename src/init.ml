open Yojson.Basic.Util
open Util

exception UndefinedBall
exception UndefinedLine
exception UndefinedPocket

let data = Yojson.Basic.from_file "data/init.json"
let scale = data |> member "scale" |> to_number
let ball_radius = (data |> member "ball_radius" |> to_number) *. scale

let ball_of_json json =
  let x, y =
    (json |> member "x" |> to_number, json |> member "y" |> to_number)
  in
  let c =
    match json |> member "type" |> to_string with
    | "player1" -> Ball.Cue 1
    | "player2" -> Ball.Cue 2
    | "target" -> Ball.Yellow
    | _ -> raise UndefinedBall
  in
  Ball.init (scale *. x, scale *. y) ball_radius 0.95 c

let line_of_json json =
  let xs = json |> member "xs" |> to_list |> List.map to_number in
  let ys = json |> member "ys" |> to_list |> List.map to_number in
  let points = List.map2 Raylib.Vector2.create xs ys in
  match points with
  | [ p1; p2 ] -> LineBoundary.init (p1 <*> scale) (p2 <*> scale)
  | _ -> raise UndefinedLine

let pocket_of_json json =
  let xs = json |> member "xs" |> to_list |> List.map to_number in
  let ys = json |> member "ys" |> to_list |> List.map to_number in
  let pocket_type =
    match json |> member "type" |> to_string with
    | "player1" -> 1
    | "player2" -> 2
    | _ -> raise UndefinedPocket
  in
  let points = List.map2 Raylib.Vector2.create xs ys in
  match points with
  | [ p1; p2 ] -> Pocket.init (p1 <*> scale) (p2 <*> scale) pocket_type
  | _ -> raise UndefinedLine

let init_balls =
  data |> member "balls" |> to_list |> List.map ball_of_json

let init_line_boundaries =
  data |> member "boundaries" |> to_list |> List.map line_of_json

let init_pockets =
  data |> member "pockets" |> to_list |> List.map pocket_of_json

let window_dimensions =
  data |> member "window" |> fun x ->
  ( (x |> member "width" |> to_number) *. scale |> int_of_float,
    (x |> member "height" |> to_number) *. scale |> int_of_float )
