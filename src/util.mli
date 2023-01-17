open Raylib

val multiply_scalar : Vector2.t -> float -> Vector2.t
val ( <*> ) : Vector2.t -> float -> Vector2.t
val ( <+> ) : Vector2.t -> Vector2.t -> Vector2.t
val ( <-> ) : Vector2.t -> Vector2.t -> Vector2.t
val vec_pair : Vector2.t -> float * float
val ( <=> ) : Vector2.t -> Vector2.t -> bool
val vec_map : (float -> float) -> Vector2.t -> Vector2.t

val point_line_dist : Vector2.t -> Vector2.t -> Vector2.t -> float
(** [point_line_dist p1 p2 p0] is the shortest distance between the line
    defined by the points [p1] and [p2] and the point [p0].*)

val reflect_x : Vector2.t -> Vector2.t
(** [reflect_x v] is the vector [v] reflected over the x-axis.*)

val reflect_y : Vector2.t -> Vector2.t
(** [reflect_y v] is the vector [v] reflected over the y-axis.*)

val x_hat : unit -> Vector2.t
val y_hat : unit -> Vector2.t
val create_pair : float * float -> Vector2.t