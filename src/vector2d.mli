(** type for handling vector operations*)

type t

val add : t -> t -> t

val mult : t -> float -> t

val dot : t -> t -> float

val sub : t -> t -> t

val norm_sq : t -> float

val norm : t -> float