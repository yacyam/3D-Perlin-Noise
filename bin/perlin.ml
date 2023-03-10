open Graphics
open Linearalg
open Vector

(* THIS IS JUST A TEST OF THE GRAPHICS LIBRARY *)
let _ = open_graph ""
let () = set_window_title "Basic Starter Code"
let () = resize_window 600 600
let size = (size_x (), size_y ())

(** [distance_matrix mat n x y] creates a matrix of distances to each pixel
    scaled by 1 over [n], as [n] denotes the size of the screen. For each row of
    the matrix, each entry holds the distance from the TOP LEFT, TOP RIGHT,
    BOTTOM LEFT, and BOTTOM RIGHT based on the [x] and [y] position of pixel.

    Requires: [mat] must be an [n] by [n] matrix. *)
let rec distance_matrix mat n x y :
    (vector * vector * vector * vector) Matrix.matrix =
  let x_dist = float_of_int x /. float_of_int n in
  let y_dist = float_of_int y /. float_of_int n in
  if x >= n then distance_matrix mat n 0 (y + 1)
  else if y >= n then mat
  else
    distance_matrix
      (Matrix.add_entry y x
         ( (*TL*)
           (x_dist, y_dist -. 1.0, 0.),
           (*TR*)
           (x_dist -. 1.0, y_dist -. 1.0, 0.),
           (*BL*)
           (x_dist, y_dist, 0.),
           (*BR*)
           (x_dist -. 1.0, y_dist, 0.) )
         mat)
      n (x + 1) y

(** [basic_matrix n] creates a basic [n] by [n] matrix with each entry holding 4
    zero vectors. *)
let basic_matrix n : (vector * vector * vector * vector) Matrix.matrix =
  Matrix.basic_matrix n n
    ((0., 0., 0.), (0., 0., 0.), (0., 0., 0.), (0., 0., 0.))

(** [convert_grayscale x] takes in a float (random number generated from perlin
    noise) and outputs a grayscale rgb value based on the range the value [x]
    encompassed.*)
let convert_grayscale x =
  let scaled_x = mod_float (x *. 1000.) 255.0 in
  if scaled_x <= 13.0 then rgb 13 13 13
  else if scaled_x <= 26.0 then rgb 26 26 26
  else if scaled_x <= 40.0 then rgb 40 40 40
  else if scaled_x <= 54.0 then rgb 54 54 54
  else if scaled_x <= 69.0 then rgb 69 69 69
  else if scaled_x <= 83.0 then rgb 83 83 83
  else if scaled_x <= 96.0 then rgb 96 96 96
  else if scaled_x <= 109.0 then rgb 109 109 109
  else if scaled_x <= 123.0 then rgb 123 123 123
  else if scaled_x <= 137.0 then rgb 137 137 137
  else if scaled_x <= 151.0 then rgb 151 151 151
  else if scaled_x <= 164.0 then rgb 164 164 164
  else if scaled_x <= 178.0 then rgb 178 178 178
  else if scaled_x <= 192.0 then rgb 192 192 192
  else if scaled_x <= 205.0 then rgb 205 205 205
  else if scaled_x <= 218.0 then rgb 218 218 218
  else if scaled_x <= 231.0 then rgb 231 231 231
  else if scaled_x <= 244.0 then rgb 244 244 244
  else rgb 255 255 255

let display_entry x y n =
  let rbg_col = convert_grayscale (Random.float 100.) in
  set_color rbg_col;
  fill_rect x y n n

let rec grid x y n =
  let _ = display_entry x y n in
  if x > fst size then grid 0 (y + n) n
  else if y > snd size then ()
  else grid (x + n) y n

let () = Random.self_init ()
let () = grid 0 0 10
