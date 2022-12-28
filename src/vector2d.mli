(** type for handling vector operations*)

type t = {
  x : float;
  y : float;
}

val add : t -> t -> t
val mult : t -> float -> t
val dot : t -> t -> float
val sub : t -> t -> t
val norm_sq : t -> float
val norm : t -> float
val dist_sq : t -> t -> float
val dist : t -> t -> float

val zero : t
(** [zero] is a zero vector.*)

val angle : t -> t -> float

val vec : float -> float -> t
(** [vec x y] creates a new vector [(x, y)].*)

val vec_pair : t -> float * float
(** [vec_pair v] is [(v.x, v.y)].*)