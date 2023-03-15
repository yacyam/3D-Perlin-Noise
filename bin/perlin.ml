open Graphics
open Linearalg
open Vector

(* THIS IS JUST A TEST OF THE GRAPHICS LIBRARY *)
let _ = open_graph ""
let () = set_window_title "Basic Starter Code"
let () = resize_window 600 600
let scn_size = (size_x (), size_y ())

(** [distance_matrix mat size x y] creates a matrix of distances to each pixel
    defined by the [size] of the screen. For each row of the matrix, each entry
    holds the distance from the TOP LEFT, TOP RIGHT, BOTTOM LEFT, and BOTTOM
    RIGHT based on the [x] and [y] position of pixel.

    Requires: [mat] must be a [size] by [size] matrix. *)
let rec distance_matrix mat size x y :
    (vector * vector * vector * vector) Matrix.matrix =
  (* This helper only allows for a distance matrix of [size] by [size] to be
     created, as if we start at some x and y position on screen where it's not 0
     0, then we have to hold the initial x and y positions, and then iterate
     [size] times until our initial [x] and [y] pos are incremented by the size
     given. Helps to increase performance tenfold to not create one huge mat. *)
  let rec distance_matrix_helper mat x y x_hold y_hold =
    let x_dist = float_of_int x in
    let y_dist = float_of_int y in
    let f_size = float_of_int size in
    if y - y_hold >= size then mat
    else if x - x_hold >= size then
      (* Go up by a row *)
      distance_matrix_helper mat x_hold (y + 1) x_hold y_hold
    else
      distance_matrix_helper
        (Matrix.add_entry (y - y_hold) (x - x_hold)
           ( (*TL*)
             (x_dist, y_dist -. f_size, 0.),
             (*TR*)
             (x_dist -. f_size, y_dist +. f_size, 0.),
             (*BL*)
             (x_dist, y_dist, 0.),
             (*BR*)
             (x_dist -. f_size, y_dist, 0.) )
           mat)
        (x + 1) y x_hold y_hold
  in
  distance_matrix_helper mat x y x y

(** [basic_matrix n] creates a basic [n] by [n] matrix with each entry holding 4
    zero vectors. *)
let basic_matrix n : (vector * vector * vector * vector) Matrix.matrix =
  Matrix.basic_matrix n n
    ((0., 0., 0.), (0., 0., 0.), (0., 0., 0.), (0., 0., 0.))

(** [convert_grayscale x] takes in a float (random number generated from perlin
    noise) and outputs a grayscale rgb value based on the range the value [x]
    encompassed.*)
let convert_grayscale x =
  let scaled_x = Int.abs (int_of_float ((x +. 1.) /. 2. *. 255.0)) in
  rgb scaled_x scaled_x scaled_x

(** [display_matrix mat x y size] displays the matrix entries at the specified x
    and y coordinates on the screen, bounded by the [size] which is specified by
    matrix size. Requires: [mat].row_length and [size] are the same length *)
let display_matrix mat x y size =
  (* [y_hold] and [x_hold] will always retain the first x and y input, so that
     we can calculate when exactly the x and y are off screen, based on [size]*)
  let rec display_matrix_helper x y x_hold y_hold =
    if y_hold + size <= y then ()
    else if x_hold + size <= x then
      display_matrix_helper (x - size) (y + 1) x_hold y_hold
    else (
      set_color (Matrix.get_entry (y - y_hold) (x - x_hold) mat);
      fill_rect x y 1 1;
      display_matrix_helper (x + 1) y x_hold y_hold)
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
    let dmat = distance_matrix (basic_matrix size) size x y in
    let rgb_mat = pixel_mat (gray_matrix size) dmat 0 0 size in
    display_matrix rgb_mat x y size;
    grid (x + size) y size

let () = Random.self_init ()
let () = grid 0 0 50
