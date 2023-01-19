open Raylib.Vector2
open Cue
open Raylib
open LineBoundary
open Util
open Control
open Pocket
open Ball

type state =
  | Control
  | Moving

type t = {
  balls : Ball.t list;
  cue : Cue.t;
  line_boundaries : LineBoundary.t list;
  state : state;
  selected : Ball.t option;
  control : Control.t;
  pockets : Pocket.t list;
}

let substeps = 4
let scale = 10.

type setup_info = {
  win_width : float;
  win_height : float;
  table_width : float;
  table_height : float;
  pocket_width : float;
  pocket_height : float;
}

let setup_info =
  {
    win_width = 120. *. scale;
    win_height = 80. *. scale;
    table_width = 84. *. scale;
    table_height = 42. *. scale;
    pocket_width = 4.5 *. scale;
    pocket_height = 4.5 *. scale;
  }

let ball_radius = 1.125 *. scale
let cueball t = t.selected
let cueball_pos t = cueball t |> Option.get |> pos
let pockets t = t.pockets

let random_color () =
  match Random.int 7 with
  | 0 -> Ball.Red
  | 1 -> Ball.Yellow
  | 2 -> Ball.Green
  | 3 -> Ball.Brown
  | 4 -> Ball.Blue
  | 5 -> Ball.Pink
  | 6 -> Ball.Black
  | _ -> failwith "impossible"

let new_ball () =
  Ball.init
    (Random.float 300. +. 250., Random.float 300. +. 250.)
    ball_radius 0.96 Cue

let init_balls () =
  let rec random_ball b_list n =
    if n = 0 then b_list
    else
      let new_b = ref (new_ball ()) in
      while
        List.fold_left (fun x y -> x || touching y !new_b) false b_list
      do
        new_b := new_ball ()
      done;
      random_ball (!new_b :: b_list) (n - 1)
  in
  random_ball
    [
      Ball.init (400., 400.) ball_radius 0.96 Cue;
      Ball.init (800., 400.) ball_radius 0.96 Cue;
    ]
    50

let create_boundary_points () =
  match setup_info with
  | {
   win_width = ww;
   win_height = wh;
   table_width = tw;
   table_height = th;
   pocket_width = pw;
   pocket_height = ph;
  } ->
      let start = create ((ww -. tw) /. 2.) ((wh -. th) /. 2.) in
      let deltas =
        [
          (tw, 0.);
          (0., (th -. ph) /. 2.);
          (pw, 0.);
          (0., ph);
          (~-.pw, 0.);
          (0., (th -. ph) /. 2.);
          (~-.tw, 0.);
          (0., ~-.(th -. ph) /. 2.);
          (~-.pw, 0.);
          (0., ~-.ph);
          (pw, 0.);
          (0., ~-.(th -. ph) /. 2.);
        ]
      in
      List.fold_left
        (fun x y -> (List.hd x <+> create_pair y) :: x)
        [ start ] deltas

let init_line_boundaries () =
  let points = create_boundary_points () in
  let rec generate_boundaries = function
    | [] -> []
    | [ p ] -> [ LineBoundary.init p (List.hd points) ]
    | v1 :: v2 :: t ->
        LineBoundary.init v1 v2 :: generate_boundaries (v2 :: t)
  in
  generate_boundaries points

let init_pockets () =
  match setup_info with
  | {
   win_width = ww;
   win_height = wh;
   table_width = tw;
   table_height = th;
   pocket_width = pw;
   pocket_height = ph;
  } ->
      let r = min pw ph *. 0.4 in
      Pocket.
        [
          init (create ((ww -. tw -. pw) /. 2.) (wh /. 2.)) r 50.;
          init (create ((ww +. tw +. pw) /. 2.) (wh /. 2.)) r 50.;
        ]

let init =
  {
    balls = init_balls ();
    cue = Cue.init (400., 400.) (36. *. scale);
    line_boundaries = init_line_boundaries ();
    state = Control;
    selected = None;
    control = Control.init ();
    pockets = init_pockets ();
  }

let balls t =
  match t.selected with
  | None -> t.balls
  | Some c -> c :: t.balls

let cue t = t.cue
let line_boundaries t = t.line_boundaries

let hit_cueball b c dt =
  let a = Float.pi +. angle c in
  let v =
    Float.(
      create
        ((a |> cos) *. power c /. dt)
        ((a |> sin) *. (power c /. dt)))
  in
  b |> Option.get |> set_accel v

(** applies boundary conditions for a singular ball*)
let rec apply_boundary_ball b bl =
  match bl with
  | [] -> b
  | h :: t -> begin
      match resolve_boundary_collision h b with
      | Some b -> b
      | None -> apply_boundary_ball b t
    end

let apply_boundary bl =
  let rec apply_boundary_rec acc = function
    | [] -> acc
    | h :: t -> apply_boundary_rec (apply_boundary_ball h bl :: acc) t
  in
  apply_boundary_rec []

let check_pockets p_lst b_lst =
  let one_ball_check ball = List.filter (falls ball) p_lst in
  List.filter (fun x -> one_ball_check x = []) b_lst

let all_ball_collisions lst =
  let fold_helper x y =
    if touching (fst x) y then
      let b1, b2 = resolve_collision_elastic (fst x) y in
      (b1, snd x @ [ b2 ])
    else (fst x, snd x @ [ y ])
  in
  let rec get_collisions acc = function
    | [] -> List.rev acc
    | h :: t ->
        let i_n, t_n = List.fold_left fold_helper (h, []) t in
        get_collisions (i_n :: acc) t_n
  in
  get_collisions [] lst

let update_balls balls line_boundaries pockets =
  let i, rb = (ref substeps, ref balls) in
  while !i > 0 do
    rb := List.map (Ball.tick (1. /. float_of_int substeps)) !rb;
    rb := check_pockets pockets !rb;
    rb := apply_boundary line_boundaries !rb;
    rb := all_ball_collisions !rb;
    i := !i - 1
  done;
  !rb

let rec filter_selected mouse_pos selected prev = function
  | [] -> (selected, prev)
  | h :: t ->
      if distance mouse_pos (pos h) < Ball.radius h && color h = Cue
      then
        ( Some h,
          if selected = None then prev @ t
          else (Option.get selected :: prev) @ t )
      else filter_selected mouse_pos selected (h :: prev) t

let check_click t = function
  | Released (Click v) ->
      let selected, balls = filter_selected v t.selected [] t.balls in
      if selected = None then t else { t with selected; balls }
  | _ -> t

let check_contact t =
  if Cue.contact t.cue then
    {
      t with
      balls =
        hit_cueball t.selected t.cue (1. /. float_of_int substeps)
        :: t.balls;
      state = Moving;
      selected = None;
    }
  else t

let cue_tick t mouse_action =
  match t.selected with
  | None -> Cue.tick t.cue mouse_action None
  | Some c -> Cue.tick t.cue mouse_action (Some (pos c))

let update_state t =
  match t.state with
  | Control -> Control
  | Moving -> if List.exists moving (balls t) then Moving else Control

let tick t =
  let control =
    if t.state = Control then Control.tick t.control
    else Control.init ()
  in
  let mouse_action = Control.action control in
  let t = check_click t mouse_action in
  let cue = cue_tick t mouse_action in
  let t = check_contact { t with cue } in
  let t =
    if t.state = Moving then
      {
        t with
        balls = update_balls t.balls t.line_boundaries t.pockets;
      }
    else t
  in
  { t with control; state = update_state t }
