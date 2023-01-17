open Raylib
open Raylib.Vector2

type mouse_action =
  | None of t
  | Click of t
  | Dragging of t * t
  | Released of mouse_action

type t = {
  clicked_ticks : int;
  action : mouse_action;
}

let rec to_string = function
  | None _ -> Printf.sprintf "None\n"
  | Click v1 -> Printf.sprintf "Click at (%f, %f)\n" (x v1) (y v1)
  | Dragging (v1, v2) ->
      Printf.sprintf "Dragging from (%f, %f) to (%f, %f)\n" (x v1)
        (y v1) (x v2) (y v2)
  | Released a -> Printf.sprintf "Released a " ^ to_string a

let action t = t.action
let init () = { clicked_ticks = 0; action = None (zero ()) }

let released_action mouse_pos = function
  | Click v -> Released (Click v)
  | Dragging (v1, v2) -> Released (Dragging (v1, v2))
  | _ -> None mouse_pos

let set_dragging mouse_pos = function
  | Click _ -> Dragging (mouse_pos, mouse_pos)
  | Dragging (v1, v2) -> Dragging (v1, mouse_pos)
  | _ -> None mouse_pos

let tick t =
  let mouse_down, mouse_pos =
    (is_mouse_button_down MouseButton.Left, get_mouse_position ())
  in
  if mouse_down then
    if t.clicked_ticks = 0 then
      { clicked_ticks = 1; action = Click mouse_pos }
    else
      {
        clicked_ticks = t.clicked_ticks + 1;
        action =
          (if t.clicked_ticks > 3 then set_dragging mouse_pos t.action
          else t.action);
      }
  else
    { clicked_ticks = 0; action = released_action mouse_pos t.action }
