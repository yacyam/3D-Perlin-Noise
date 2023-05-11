open Vector

(** Implements the main perlin noise random value generation with matrices *)

val basic_dist_mat : int -> vector Matrix.t
(** [basic_dist_mat n] creates a basic [n] by [n] matrix with each entry holding
    the zero vector. *)

val normalize_dist : vector -> float * float * float
(** [normalize_dist (x, y, z)] is for normalizing the lengths of the distance
    vectors to be scaled down by a factor of sqrt 2 *)

val distance_matrix : vector Matrix.t -> int -> int -> int -> vector Matrix.t
(** [distance_matrix mat size x y] creates a matrix of distances to each pixel
    defined by the [size] of the screen. Each entry of the matrix [mat] holds
    the distance relative to the starting [x] and [y] position. Requires: [mat]
    must be a [size] by [size] matrix. *)

val smooth : float -> float
(** [smooth n] is the result of apply the function (6n^5) - (15n^4) + (10n^3) to
    [n]. The resulting n will be used in interpolation.*)

val interpolate : float -> float -> float -> float -> float -> float -> float
(** [interpolate upper_left upper_right lower_left lower_right frac_x frac_y] is
    a linear interpolation of dot products of each corner of a pixel. *)

val dot_grad_dist : int -> vector -> float
(** [dot_grad_dist random distance_vector] is the dot product of a
    [distane_vector] and a gradient vector that is chosen based on [random]
    value when called in [gradient_of_pixel]. *)

val gradient_of_pixel_fbm : float -> int array -> vector -> float
(** [gradient_of_pixel_fbm freq random_values pixel_pos] is the final color
    value of the pixel specified by [pixel_pos], after applying the linear
    interpolation and smoothing based on the gradient vectors specified through
    [random_values]. *)
