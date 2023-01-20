open Raylib.Vector2
open Cue
open Raylib
open LineBoundary
open Util
open Control
open Pocket
open Ball

type state =
  | Control of int
  | Moving of int

type t = {
  balls : Ball.t list;
  cue : Cue.t;
  line_boundaries : LineBoundary.t list;
  state : state;
  selected : Ball.t option;
  control : Control.t;
  pockets : Pocket.t list;
  score : int * int;
}

let substeps = 4
let cueball t = t.selected
let cueball_pos t = cueball t |> Option.get |> pos
let pockets t = t.pockets
let score t = t.score
let cue t = t.cue
let line_boundaries t = t.line_boundaries

let current_player t =
  match t.state with
  | Control i | Moving i -> i

let init =
  {
    balls = Init.init_balls;
    cue = Cue.init (400., 400.) (36. *. Init.scale);
    line_boundaries = Init.init_line_boundaries;
    state = Control 1;
    selected = None;
    control = Control.init ();
    pockets = Init.init_pockets;
    score = (0, 0);
  }

let balls t =
  match t.selected with
  | None -> t.balls
  | Some c -> c :: t.balls

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
  let one_ball_check ball = List.filter (is_goal ball) p_lst in
  List.map (fun x -> if one_ball_check x = [] then x else sink x) b_lst

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

let rec filter_selected mouse_pos selected prev player = function
  | [] -> (selected, prev)
  | h :: t ->
      if distance mouse_pos (pos h) < Ball.radius h && is_cue h player
      then
        ( Some h,
          if selected = None then prev @ t
          else (Option.get selected :: prev) @ t )
      else filter_selected mouse_pos selected (h :: prev) player t

let check_click t = function
  | Released (Click v) ->
      let selected, balls =
        filter_selected v t.selected [] (current_player t) t.balls
      in
      if selected = None then t else { t with selected; balls }
  | _ -> t

let check_contact t =
  if Cue.contact t.cue then
    {
      t with
      balls =
        hit_cueball t.selected t.cue (1. /. float_of_int substeps)
        :: t.balls;
      state =
        (match t.state with
        | Control i -> Moving i
        | _ -> failwith "checking contact while moving");
      selected = None;
    }
  else t

let cue_tick t mouse_action =
  match t.selected with
  | None -> Cue.tick t.cue mouse_action None
  | Some c -> Cue.tick t.cue mouse_action (Some (pos c))

let update_state t =
  match t.state with
  | Control i -> Control i
  | Moving i ->
      if List.exists moving (balls t) then Moving i
      else Control (if i = 2 then 1 else 2)

let tick t =
  let control =
    match t.state with
    | Control _ -> Control.tick t.control
    | _ -> Control.init ()
  in
  let mouse_action = Control.action control in
  let t = check_click t mouse_action in
  let cue = cue_tick t mouse_action in
  let t = check_contact { t with cue } in
  let t =
    match t.state with
    | Moving _ ->
        {
          t with
          balls = update_balls t.balls t.line_boundaries t.pockets;
        }
    | _ -> t
  in
  { t with control; state = update_state t }
