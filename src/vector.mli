(** Implements main vector operations for 3-dimensional vectors. *)

type vector = float * float * float
(** [vector] represents a 3-dimensional vector *)

val get_x : vector -> float
(** [get_x (x, y, z)] is [x] *)

val get_y : vector -> float
(** [get_y (x, y, z)] is [y] *)

val get_z : vector -> float
(** [get_z (x, y, z)] is [z] *)

val dot : vector -> vector -> float
(** [dot (x, y, z) (a, b, c)] is the dot product of the two input vectors *)

val cross : vector -> vector -> vector
(** [cross (x, y, z) (a, b, c)] is the cross product of the two input vectors *)

val magnitude : vector -> float
(** [magnitude (x, y, z)] is the length of the input vector *)

val norm : vector -> vector
(** [norm (x, y, z)] is the unit normal vector that points in the same direction
    as the input vector. Requires: The magnitude is not 0. *)

(*val angle : vector -> vector -> float*)

val to_string : vector -> string
(** [to_string (x, y, z)] is "(x, y, z)" *)
