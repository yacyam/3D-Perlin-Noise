type vector = float * float * float

(** [get_x (x, y, z)] is x (float) *)
let get_x (x, y, z) = x

(** [get_y (x, y, z)] is y (float) *)
let get_y (x, y, z) = y

(** [get_z (x, y, z)] is z (float) *)
let get_z (x, y, z) = z

(** [dot (x, y, z) (a, b, c)] is the dot product of two vectors (float) *)
let dot (x, y, z) (a, b, c) = (x *. a) +. (y *. b) +. (z *. c)

(** [cross (x, y, z) (a, b, c)] is the cross product of two vectors (float) *)
let cross (x, y, z) (a, b, c) =
  ((y *. c) -. (z *. b), (z *. a) -. (x *. c), (x *. b) -. (y *. a))

(** [magnitude (x, y, z)] outputs length of given vector (float) *)
let magnitude (x, y, z) = sqrt ((x *. x) +. (y *. y) +. (z *. z))

(** [norm (x, y, z)] returns the unit normal vector that points in the same
    direction as the input vector. Requires the magnitude is not 0. *)
let norm (x, y, z) =
  (x /. magnitude (x, y, z), y /. magnitude (x, y, z), z /. magnitude (x, y, z))

(*(** [angle (x, y, z) (a, b, c)] Finds the angle between the two vectors (float
  between 0 and pi in radians). *) let angle (x, y, z) (a, b, c) = dot (x, y, z)
  (a, b, c) /. (magnitude (x, y, z) +. magnitude (a, b, c)) |> acos*)

let to_string (x, y, z) =
  "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ", " ^ string_of_float z
  ^ ")"
