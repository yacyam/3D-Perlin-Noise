open Graphics
open Linearalg
open Vector

(* THIS IS JUST A TEST OF THE GRAPHICS LIBRARY *)
let _ = open_graph ""
let () = set_window_title "Basic Starter Code"
let () = resize_window 600 600
let scn_size = (size_x (), size_y ())

(** [distance_matrix mat n x y] creates a matrix of distances to each pixel
    scaled by 1 over [n], as [n] denotes the size of the screen. For each row of
    the matrix, each entry holds the distance from the TOP LEFT, TOP RIGHT,
    BOTTOM LEFT, and BOTTOM RIGHT based on the [x] and [y] position of pixel.

    Requires: [mat] must be an [n] by [n] matrix. *)
let rec distance_matrix mat size x y :
    (vector * vector * vector * vector) Matrix.matrix =
  let x_dist = float_of_int x in
  let y_dist = float_of_int y in
  if y >= size then mat
  else if x >= size then distance_matrix mat size 0 (y + 1)
  else
    distance_matrix
      (Matrix.add_entry y x
         ( (*TL*)
           (x_dist, y_dist -. 1.0, 0.),
           (*TR*)
           (x_dist -. 1.0, y_dist +. 1.0, 0.),
           (*BL*)
           (x_dist, y_dist, 0.),
           (*BR*)
           (x_dist -. 1.0, y_dist, 0.) )
         mat)
      size (x + 1) y

(** [basic_matrix n] creates a basic [n] by [n] matrix with each entry holding 4
    zero vectors. *)
let basic_matrix n : (vector * vector * vector * vector) Matrix.matrix =
  Matrix.basic_matrix n n
    ((0., 0., 0.), (0., 0., 0.), (0., 0., 0.), (0., 0., 0.))

(** [convert_grayscale x] takes in a float (random number generated from perlin
    noise) and outputs a grayscale rgb value based on the range the value [x]
    encompassed.*)
let convert_grayscale x =
  let scaled_x = mod_float (x *. 10000.) 255.0 in
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

(** [display_matrix mat x y size] displays the matrix entries at the specified x
    and y coordinates on the screen, bounded by the [size] which is specified by
    matrix size. Requires: [mat].row_length and [size] are the same length *)
let display_matrix mat x y size =
  (* [y_hold] and [x_hold] will always retain the first x and y input, so that
     we can calculate when exactly the x and y are off screen, based on [size]*)
  let rec display_matrix_helper x y x_hold y_hold =
    if y_hold + size <= y then ()
    else if x_hold + size <= x then
      display_matrix_helper (x - size) (y + 5) x_hold y_hold
    else (
      set_color (Matrix.get_entry (y - y_hold) (x - x_hold) mat);
      fill_rect x y 5 5;
      display_matrix_helper (x + 5) y x_hold y_hold)
  in
  display_matrix_helper x y x y

let gray_matrix n = Matrix.basic_matrix n n (rgb 255 255 255)

(** [pixel_mat rgb_mat d_mat x y size] returns a matrix [rgb_mat] that's the
    Main.ml noise function applied to the distance vectors in each entry of
    [d_mat] *)
let rec pixel_mat rgb_mat d_mat x y size =
  if x >= size then pixel_mat rgb_mat d_mat 0 (y + 1) size
  else if y >= size then rgb_mat
  else
    pixel_mat
      (Matrix.add_entry y x
         (Matrix.get_entry y x d_mat |> Main.gradient_of_pixel
        |> convert_grayscale)
         rgb_mat)
      d_mat (x + 1) y size

(** [grid x y size] creates a grid of size [size] on the screen starting from
    the [x] and [y] positions until the entire screen size is filled. *)
let rec grid x y size =
  if y >= snd scn_size then ()
  else if x >= fst scn_size then grid 0 (y + size) size
  else
    let dmat = distance_matrix (basic_matrix size) size 0 0 in
    let rgb_mat = pixel_mat (gray_matrix size) dmat 0 0 size in
    display_matrix rgb_mat x y size;
    grid (x + size) y size

let () = Random.self_init ()
let () = grid 0 0 600
