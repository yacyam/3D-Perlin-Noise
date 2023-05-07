(** Implements the main perlin noise random value generation *)

open Matrix
open Vector

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
