open Float
open Raylib
open Raylib.Vector2
open Util

type line_type =
  | Vertical
  | Horizontal
  | General

type collision_info = {
  collision : bool;
  depth : float;
  corner : bool;
  collision_point : Vector2.t;
}

let no_collision =
  {
    collision = false;
    depth = 0.;
    corner = false;
    collision_point = zero ();
  }

type t = {
  line_type : line_type;
  p1 : Vector2.t;
  p2 : Vector2.t;
}

let init p1 p2 =
  let x1, y1 = vec_pair p1 in
  let x2, y2 = vec_pair p2 in
  if x1 = x2 then { line_type = Vertical; p1; p2 }
  else if y1 = y2 then { line_type = Horizontal; p1; p2 }
  else { line_type = General; p1; p2 }

let points = function
  | { line_type; p1; p2 } -> (p1, p2)

let line_type t = t.line_type

let line_dist p = function
  | { line_type; p1; p2 } -> (
      match line_type with
      | Vertical -> abs (Vector2.x p1 -. Vector2.x p)
      | Horizontal -> abs (Vector2.y p1 -. Vector2.y p)
      | General -> point_line_dist p1 p2 p)

let lies_between p = function
  | { line_type; p1; p2 } -> begin
      match line_type with
      | Vertical -> (y p -. y p1) *. (y p -. y p2) < 0.
      | Horizontal -> (x p -. x p1) *. (x p -. x p2) < 0.
      | General ->
          not
            (is_obtuse (distance p1 p2) (distance p1 p) (distance p p2))
    end

let get_collision_point b = function
  | { line_type; p1; p2 } -> begin
      match line_type with
      | Vertical -> create (x p1) (Ball.pos b |> y)
      | Horizontal -> create (Ball.pos b |> x) (y p1)
      | General -> failwith "todo"
    end

let check_corner_collision p r p1 p2 =
  let dist = distance p1 p in
  if dist < r then
    {
      collision = true;
      depth = r -. dist;
      corner = true;
      collision_point = p1;
    }
  else
    let dist = distance p2 p in
    if dist < r then
      {
        collision = true;
        depth = r -. dist;
        corner = true;
        collision_point = p2;
      }
    else no_collision

let check_collision b check_corner line =
  let p1, p2 = points line in
  let r, ball_p = Ball.(radius b, pos b) in
  if lies_between ball_p line then
    let dist = line_dist ball_p line in
    {
      collision = r >= dist;
      depth = r -. dist;
      corner = false;
      collision_point = get_collision_point b line;
    }
  else if check_corner then check_corner_collision ball_p r p1 p2
  else no_collision
