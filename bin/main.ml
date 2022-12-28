open Game
open Snooker
open Graphics
open Raylib

let rec game_loop t =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      draw t;
      game_loop (tick t)

let () =
  Random.self_init ();
  let open Raylib.Rectangle in
  let open Raylib.Texture in
  init_window 800 800 "snooker";
  game_loop init
