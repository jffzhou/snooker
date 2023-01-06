open Game
open Snooker
open Graphics
open Raylib

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
  init_window 800 800 "snooker";
  set_target_fps 60;
  game_loop init
