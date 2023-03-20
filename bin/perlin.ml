open Graphics
open Linearalg
open Vector

let _ = open_graph ""
let () = set_window_title "Perlin Noise"
let () = resize_window 1000 600
let scn_size = (size_x () - 400, size_y ())

(** [normalize size (p1, p2, p3, p4)] is for normalizing the lengths of the
    distance vectors to be in range 0..1 *)
let normalize size (p1, p2, p3, p4) =
  ( (get_x p1 /. sqrt 2., get_y p1 /. sqrt 2., get_z p1 /. sqrt 2.),
    (get_x p2 /. sqrt 2., get_y p2 /. sqrt 2., get_z p2 /. sqrt 2.),
    (get_x p3 /. sqrt 2., get_y p3 /. sqrt 2., get_z p3 /. sqrt 2.),
    (get_x p4 /. sqrt 2., get_y p4 /. sqrt 2., get_z p4 /. sqrt 2.) )

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
           (( (*TL*)
              (x_dist, y_dist -. f_size, 0.),
              (*TR*)
              (x_dist -. f_size, y_dist +. f_size, 0.),
              (*BL*)
              (x_dist, y_dist, 0.),
              (*BR*)
              (x_dist -. f_size, y_dist, 0.) )
           |> normalize size)
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
  let scaled_x = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.0)) in
  rgb scaled_x scaled_x scaled_x

(** [convert_bluegreenscale x] takes in a float (random number generated from
    perlin noise) and outputs a blue-green rgb scaled value based on [x]. x
    should be in 0..1 or very close to it. *)
let convert_bluegreenscale x =
  let x_prime = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.)) in
  rgb 0 x_prime (255 - x_prime)

(** [convert_landmass x] takes in a float (random number generated from perlin
    noise) and outputs a blue or green rgb scaled value based on [x]. x should
    be in 0..1 or very close to it. If x is small enough, it outputs a blue
    scale value, otherwise it's green scale. *)
let convert_landscape x =
  let x_prime = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.)) in
  if x_prime < 100 then
    rgb 0 0
      (40 + int_of_float (Float.round (float_of_int x_prime *. (215. /. 100.))))
  else if x_prime > 110 then
    rgb
      (int_of_float
         (10. -. Float.round (float_of_int (x_prime - 110) *. (10. /. 145.))))
      (int_of_float
         (Float.round
            (160. -. (float_of_int (x_prime - 110) *. (160. /. 145.)) +. 50.)))
      (int_of_float
         (60. -. Float.round (float_of_int (x_prime - 110) *. (60. /. 145.))))
  else
    let r, g, b =
      ( Int.abs
          (int_of_float
             (((float_of_int x_prime -. 100.) *. (60. /. 10.)) +. 180.)),
        Int.abs
          (int_of_float
             (((float_of_int x_prime -. 100.) *. (40. /. 10.)) +. 188.)),
        Int.abs
          (int_of_float
             (((float_of_int x_prime -. 100.) *. (20. /. 10.)) +. 153.)) )
    in
    rgb r g b

(** [convert_brownscale x] takes in a float (random number generated from perlin
    noise) and outputs a brown rgb scaled value based on [x]. x should be in
    0..1 or very close to it. *)
let convert_wood x =
  let x_prime = (x +. 0.95) /. 2. in
  if int_of_float (Float.round (x_prime *. 100.)) mod 4 = 0 then rgb 166 99 64
  else
    let r, g, b =
      ( Int.abs (int_of_float (x_prime *. 166.)),
        Int.abs (int_of_float (x_prime *. 99.)),
        Int.abs (int_of_float (x_prime *. 64.)) )
    in
    rgb r g b

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

let draw_main_text x y =
  set_color blue;
  moveto (x - 5) (y + 180);
  (* https://openclassrooms.com/forum/sujet/ocaml-changer-la-taille-du-texte-avec-graphics
     For some reason text_size was never implemented, so you have to use a
     specific font string to change the size *)
  set_font "-*-fixed-medium-r-normal--70-*-*-*-*-*-iso8859-1";
  draw_string "CHOOSE";
  moveto (x + 20) (y + 130);
  draw_string "YOUR";
  moveto (x + 10) (y + 80);
  draw_string "NOISE";

  (* Exit Button *)
  set_color black;
  fill_rect (size_x () - 100) 10 80 50;
  set_color white;
  moveto (size_x () - 87) 22;
  set_font "-*-fixed-medium-r-normal--25-*-*-*-*-*-iso8859-1";
  draw_string "EXIT"

(** [draw_interface x y] draws the text interface and interactive buttons
    starting at the [x] and [y] positions *)
let draw_interface x y text =
  (* Perlin Button *)
  set_color red;
  draw_rect x y 80 50;
  moveto (x + 12) (y + 30);
  set_font "-*-fixed-medium-r-normal--15-*-*-*-*-*-iso8859-1";
  draw_string "Perlin";
  moveto (x + 1) (y + 10);
  draw_string text;
  (* Fractal Button *)
  set_color magenta;
  moveto (x + 111) (y + 30);
  draw_rect (x + 100) y 80 50;
  draw_string "Fractal";
  moveto (x + 101) (y + 10);
  draw_string text

(** [in_range x y start_x start_y] checks if x and y are within a 80 by 50
    (button size) rectangle range starting at [start_x] and [start_y] *)
let in_range x y start_x start_y =
  x >= start_x && x <= start_x + 80 && y >= start_y && y <= start_y + 50

let rec fbm acc d_mat y x freq amp n_octaves =
  if n_octaves = 0 then acc
  else
    let value =
      Matrix.get_entry y x d_mat
      |> Main.gradient_of_pixel_fbm freq
      |> ( *. ) amp |> ( +. ) acc
    in
    fbm value d_mat y x (freq *. 2.) (amp *. 0.5) (n_octaves - 1)

(** [pixel_mat_fbm rgb_mat d_mat x y size freq amp] returns a matrix [rgb_mat]
    that's the Main.ml noise function applied to the distance vectors in each
    entry of [d_mat] *)
let rec pixel_mat_fbm rgb_mat d_mat x y size n_octaves colorize =
  if x >= size then
    pixel_mat_fbm rgb_mat d_mat 0 (y + 1) size n_octaves colorize
  else if y >= size then rgb_mat
  else
    pixel_mat_fbm
      (Matrix.add_entry y x
         (fbm 0. d_mat y x 0.005 1. n_octaves |> colorize)
         rgb_mat)
      d_mat (x + 1) y size n_octaves colorize

(** [grid x y size] creates a grid of size [size] on the screen starting from
    the [x] and [y] positions until the specified screen size is filled. *)
let rec grid_fbm x y size n_octaves colorize =
  if y >= snd scn_size then (
    draw_main_text 700 300;
    draw_interface 700 300 "  Noise";
    draw_interface 700 240 " Colored";
    draw_interface 700 180 "Landscape";
    draw_interface 700 120 "  Wood";
    let rec loop _ =
      match wait_next_event [ Button_down ] with
      | { mouse_x; mouse_y } ->
          if in_range mouse_x mouse_y 700 300 then (
            (* Perlin Noise *)
            clear_graph ();
            grid_fbm 0 0 size 2 convert_grayscale)
          else if in_range mouse_x mouse_y 800 300 then (
            (* Fractal Noise *)
            clear_graph ();
            grid_fbm 0 0 size 6 convert_grayscale)
          else if in_range mouse_x mouse_y 700 240 then (
            (* Perlin Colored *)
            clear_graph ();
            grid_fbm 0 0 size 2 convert_bluegreenscale)
          else if in_range mouse_x mouse_y 800 240 then (
            (* Fractal Colored *)
            clear_graph ();
            grid_fbm 0 0 size 6 convert_bluegreenscale)
          else if in_range mouse_x mouse_y 700 180 then (
            (* Perlin Landscape *)
            clear_graph ();
            grid_fbm 0 0 size 2 convert_landscape)
          else if in_range mouse_x mouse_y 800 180 then (
            (* Fractal Landscape *)
            clear_graph ();
            grid_fbm 0 0 size 6 convert_landscape)
          else if in_range mouse_x mouse_y 700 120 then (
            (* Perlin Wood *)
            clear_graph ();
            grid_fbm 0 0 size 2 convert_wood)
          else if in_range mouse_x mouse_y 800 120 then (
            (* Fractal Wood *)
            clear_graph ();
            grid_fbm 0 0 size 6 convert_wood)
          else if in_range mouse_x mouse_y (size_x () - 100) 10 then
            close_graph ()
          else loop ()
    in
    loop ())
  else if x >= fst scn_size then grid_fbm 0 (y + size) size n_octaves colorize
  else
    let dmat = distance_matrix (basic_matrix size) size x y in
    let rgb_mat =
      pixel_mat_fbm (gray_matrix size) dmat 0 0 size n_octaves colorize
    in
    display_matrix rgb_mat x y size;
    grid_fbm (x + size) y size n_octaves colorize

let () = Random.self_init ()
let () = grid_fbm 0 0 50 2 convert_grayscale
