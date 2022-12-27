type t = {
  x : float;
  y : float;
}

let add v1 v2 = {x = v1.x +. v2.x; y = v1.y +. v2.y}

let mult v1 s = {x = v1.x*.s; y = v1.y*.s}

let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y

let sub v1 v2 = mult v2 (-1.) |> add v1

let norm_sq v = v.x ** 2. +. v.y ** 2.

let norm v = sqrt (norm_sq v)