type t

type setup_info = {
  win_width : float;
  win_height : float;
  table_width : float;
  table_height : float;
  goal_width : float;
  goal_height : float;
}

val setup_info : setup_info
val ball_radius : float
val cueball : t -> Ball.t option
val init : t
val tick : t -> t
val balls : t -> Ball.t list
val cue : t -> Cue.t
val line_boundaries : t -> LineBoundary.t list