open Game
open Snooker
open Graphics
open Raylib
open Init

let debug = false

let rec game_loop t =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      draw t;
      if debug then (
        while is_key_up Key.Space do
          draw t
        done;
        while is_key_down Key.Space do
          draw t
        done)
      else ();
      game_loop (tick t)

let () =
  Random.self_init ();
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  let width, height = window_dimensions in
  init_window width height "snooker";
  set_target_fps 60;
  game_loop init
