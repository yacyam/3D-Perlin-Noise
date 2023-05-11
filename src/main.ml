open Matrix
open Vector

(** [basic_dist_mat n] creates a basic [n] by [n] matrix with each entry holding
    the zero vector. *)
let basic_dist_mat n : vector Matrix.t = Matrix.basic_matrix n n (0., 0., 0.)

(** [normalize_dist (x, y, z)] is for normalizing the lengths of the distance
    vectors to be scaled down by a factor of sqrt 2 *)
let normalize_dist p1 =
  (get_x p1 /. sqrt 2., get_y p1 /. sqrt 2., get_z p1 /. sqrt 2.)

(** [distance_matrix mat size x y] creates a matrix of distances to each pixel
    defined by the [size] of the screen. Each entry of the matrix [mat] holds
    the distance relative to the starting [x] and [y] position. Requires: [mat]
    must be a [size] by [size] matrix. *)
let rec distance_matrix mat size x y : vector Matrix.t =
  (* This helper only allows for a distance matrix of [size] by [size] to be
     created, as if we start at some x and y position on screen where it's not 0
     0, then we have to hold the initial x and y positions, and then iterate
     [size] times until our initial [x] and [y] pos are incremented by the size
     given. Helps to increase performance tenfold to not create one huge mat. *)
  let rec distance_matrix_helper mat x y x_hold y_hold =
    let x_dist = float_of_int x in
    let y_dist = float_of_int y in
    if y - y_hold >= size then mat
    else if x - x_hold >= size then
      (* Go up by a row *)
      distance_matrix_helper mat x_hold (y + 1) x_hold y_hold
    else
      distance_matrix_helper
        (Matrix.add_entry (y - y_hold) (x - x_hold)
           ((x_dist, y_dist, 0.) |> normalize_dist)
           mat)
        (x + 1) y x_hold y_hold
  in
  distance_matrix_helper mat x y x y

(** [smooth n] is the result of apply the function (6n^5) - (15n^4) + (10n^3) to
    [n]. The resulting n will be used in interpolation.*)
let smooth n =
  let poly_1 = n *. n *. n *. n *. n *. 6.0 in
  let poly_2 = n *. n *. n *. n *. 15.0 in
  let poly_3 = n *. n *. n *. 10.0 in
  poly_1 -. poly_2 +. poly_3

(** [interpolate upper_left upper_right lower_left lower_right frac_x frac_y] is
    a linear interpolation of dot products of each corner of a pixel. *)
let interpolate upper_left upper_right lower_left lower_right frac_x frac_y =
  let smooth_x = smooth frac_x in
  let smooth_y = smooth frac_y in
  let interpolate_1 =
    upper_right -. upper_left |> ( *. ) smooth_x |> ( +. ) upper_left
  in
  let interpolate_2 =
    lower_right -. lower_left |> ( *. ) smooth_x |> ( +. ) lower_left
  in
  interpolate_1 -. interpolate_2 |> ( *. ) smooth_y |> ( +. ) interpolate_2

(** [dot_grad_dist random distance_vector] is the dot product of a
    [distane_vector] and a gradient vector that is chosen based on [random]
    value when called in [gradient_of_pixel]. *)
let dot_grad_dist (random : int) (distance_vector : vector) : float =
  if random = 0 then dot (1.0, 1.0, 0.0) distance_vector
  else if random = 1 then dot (-1.0, 1.0, 0.0) distance_vector
  else if random = 2 then dot (1.0, -1.0, 0.0) distance_vector
  else dot (-1.0, -1.0, 0.0) distance_vector

(** [gradient_of_pixel_fbm freq random_values pixel_pos] is the final color
    value of a pixel. It takes in [pixel_pos] which is a vector of a value that
    the user enters and this vector will be the grid "location". It then takes
    the fractional parts of this to get the distance vector from a random
    gradient vector on the grid to a certain pixel on the grid. It does this
    four times for each corner and takes dot product for each and then
    interpolates to get a final value. *)
let gradient_of_pixel_fbm freq random_values pixel_pos =
  let x_float = get_x pixel_pos *. freq in
  let y_float = get_y pixel_pos *. freq in
  let x_pos = Int.abs (Float.to_int x_float) in
  let y_pos = Int.abs (Float.to_int y_float) in
  (* TL *)
  let g1 = random_values.(x_pos mod 256) in
  let g1_final = random_values.((g1 + y_pos + 1) mod 256) mod 4 in
  (* TR *)
  let g2 = random_values.((x_pos + 1) mod 256) in
  let g2_final = random_values.((g2 + y_pos + 1) mod 256) mod 4 in
  (* BL *)
  let g3 = random_values.(x_pos mod 256) in
  let g3_final = random_values.((g3 + y_pos) mod 256) mod 4 in
  (* BR *)
  let g4 = random_values.((x_pos + 1) mod 256) in
  let g4_final = random_values.((g4 + y_pos) mod 256) mod 4 in
  let frac_x = x_float -. Float.floor x_float in
  let frac_y = y_float -. Float.floor y_float in
  let d1 = dot_grad_dist g1_final (frac_x, frac_y -. 1., 0.) in
  let d2 = dot_grad_dist g2_final (frac_x -. 1., frac_y -. 1., 0.) in
  let d3 = dot_grad_dist g3_final (frac_x, frac_y, 0.) in
  let d4 = dot_grad_dist g4_final (frac_x -. 1., frac_y, 0.) in
  interpolate d1 d2 d3 d4 frac_x frac_y
