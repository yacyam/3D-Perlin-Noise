open Raylib
open Linearalg
open Vector

module RTG = struct
  (** [set_state x] sets the pseudo-random state based on the input seed [x] *)
  let set_state x =
    let new_state = Random.State.make [| x |] in
    Random.set_state new_state

  (** [gen_random_table] tail recursively generates a random table of size 256
      with integers in range 0-255. *)
  let gen_random_table seed =
    set_state seed;
    let rec loop acc = function
      | 0 -> acc
      | size ->
          loop
            (acc.(size - 1) <- int_of_float (floor (Random.float 255.9));
             acc)
            (size - 1)
    in
    loop (Array.make 256 0) 256
end

(** [normalize size (p1, p2, p3, p4)] is for normalizing the lengths of the
    distance vectors to be in range 0..1 *)
let normalize size p1 =
  (get_x p1 /. sqrt 2., get_y p1 /. sqrt 2., get_z p1 /. sqrt 2.)

(** [distance_matrix mat size x y] creates a matrix of distances to each pixel
    defined by the [size] of the screen. Each entry of the matrix [mat] holds
    the distance relative to the starting [x] and [y] position.

    Requires: [mat] must be a [size] by [size] matrix. *)
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
           ((x_dist, y_dist, 0.) |> normalize size)
           mat)
        (x + 1) y x_hold y_hold
  in
  distance_matrix_helper mat x y x y

(** [basic_matrix n] creates a basic [n] by [n] matrix with each entry holding
    the zero vector. *)
let basic_matrix n : vector Matrix.t = Matrix.basic_matrix n n (0., 0., 0.)

(** [convert_grayscale x] takes in a float (random number generated from perlin
    noise) and outputs a grayscale rgb value based on the range the value [x]
    encompassed.*)
let convert_grayscale x =
  let scaled_x = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.0)) in
  Color.create scaled_x scaled_x scaled_x 255

(** [convert_bluegreenscale x] takes in a float (random number generated from
    perlin noise) and outputs a blue-green rgb scaled value based on [x]. x
    should be in 0..1 or very close to it. *)
let convert_bluegreenscale x =
  let x_prime = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.)) in
  Color.create 0 x_prime (255 - x_prime) 255

(** [convert_landmass x] takes in a float (random number generated from perlin
    noise) and outputs a blue or green rgb scaled value based on [x]. x should
    be in 0..1 or very close to it. If x is small enough, it outputs a blue
    scale value, otherwise it's green scale. *)
let convert_landscape x =
  let open Color in
  let x_prime = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.)) in
  if x_prime < 100 then
    create 0 0
      (40 + int_of_float (Float.round (float_of_int x_prime *. (215. /. 100.))))
      255
  else if x_prime > 110 then
    create
      (int_of_float
         (10. -. Float.round (float_of_int (x_prime - 110) *. (10. /. 145.))))
      (int_of_float
         (Float.round
            (160. -. (float_of_int (x_prime - 110) *. (160. /. 145.)) +. 50.)))
      (int_of_float
         (60. -. Float.round (float_of_int (x_prime - 110) *. (60. /. 145.))))
      255
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
    create r g b 255

(** [convert_wood x] takes in a float (random number generated from perlin
    noise) and outputs a brown rgb scaled value based on [x]. x should be in
    0..1 or very close to it. *)
let convert_wood x =
  let open Color in
  let x_prime = (x +. 0.95) /. 2. in
  if int_of_float (Float.round (x_prime *. 100.)) mod 4 = 0 then
    create 166 99 64 255
  else
    let r, g, b =
      ( Int.abs (int_of_float (x_prime *. 166.)),
        Int.abs (int_of_float (x_prime *. 99.)),
        Int.abs (int_of_float (x_prime *. 64.)) )
    in
    create r g b 255

let gray_matrix n = Matrix.basic_matrix n n (Color.create 255 255 255 255)

let rec fbm acc d_mat y x freq amp n_octaves rand_vals =
  if n_octaves = 0 then acc
  else
    let value =
      Matrix.get_entry y x d_mat
      |> Main.gradient_of_pixel_fbm freq rand_vals
      |> ( *. ) amp |> ( +. ) acc
    in
    fbm value d_mat y x (freq *. 2.) (amp *. 0.5) (n_octaves - 1) rand_vals

(** [pixel_mat_fbm rgb_mat d_mat x y size freq amp] returns a matrix [rgb_mat]
    that's the Main.ml noise function applied to the distance vectors in each
    entry of [d_mat] *)
let rec pixel_mat_fbm rgb_mat d_mat x y size n_octaves colorize rand_vals =
  if x >= size then
    pixel_mat_fbm rgb_mat d_mat 0 (y + 1) size n_octaves colorize rand_vals
  else if y >= size then rgb_mat
  else
    pixel_mat_fbm
      (Matrix.add_entry y x
         (fbm 0. d_mat y x 0.005 1. n_octaves rand_vals |> colorize)
         rgb_mat)
      d_mat (x + 1) y size n_octaves colorize rand_vals

(** [grid_fbm x y size] is a color matrix of dimensions [size] by [size] with
    entries starting from the [x] and [y] positions. *)
let rec grid_fbm x y size n_octaves colorize rand_vals =
  let dmat = distance_matrix (basic_matrix size) size x y in
  pixel_mat_fbm (gray_matrix size) dmat 0 0 size n_octaves colorize rand_vals

(** [cam_setup ()] is the starting static camera position/angle *)
let cam_setup () =
  let open Raylib in
  init_window 1000 600 "3D Perlin Noise";
  let camera =
    Camera.create
      (Vector3.create 350.0 200.0 620.0) (* position *)
      (Vector3.create 300.0 350.0 ~-.70.0) (* target *)
      (Vector3.create 0.0 1.0 50.0) (* up *)
      90.0 (* FOV *) CameraProjection.Perspective
  in
  set_target_fps 60;
  camera

(** [draw_perlin camera mat res] draws the perlin noise on a three-dimensional
    axis based on the colors specified by [mat] and the starting [camera]
    position with the specified resolution [res] *)
let draw_perlin camera mat res =
  begin_drawing ();
  clear_background Color.skyblue;
  begin_mode_3d camera;
  let rec draw_seq x y =
    if y >= 600.0 then ()
    else if x >= 600.0 then draw_seq 0. (y +. res)
    else
      let mat_entry = Matrix.get_entry (int_of_float x) (int_of_float y) mat in
      draw_cube (Vector3.create x y 0.0) res res
        (Color.g mat_entry |> float_of_int)
        mat_entry;
      draw_seq (x +. res) y
  in
  draw_seq 0.0 0.0;
  end_mode_3d ()

(** [input_seed] is the string representation of the user inputted seed *)
let input_seed = ref ""

(** [update_seed_input ()] mutably updates the string holding the user inputted
    seed *)
let update_seed_input () =
  if String.length !input_seed < 7 then
    match get_key_pressed () with
    | Key.Zero -> input_seed := !input_seed ^ "0"
    | Key.One -> input_seed := !input_seed ^ "1"
    | Key.Two -> input_seed := !input_seed ^ "2"
    | Key.Three -> input_seed := !input_seed ^ "3"
    | Key.Four -> input_seed := !input_seed ^ "4"
    | Key.Five -> input_seed := !input_seed ^ "5"
    | Key.Six -> input_seed := !input_seed ^ "6"
    | Key.Seven -> input_seed := !input_seed ^ "7"
    | Key.Eight -> input_seed := !input_seed ^ "8"
    | Key.Nine -> input_seed := !input_seed ^ "9"
    | _ -> ()

(** [input_res] is the starting resolution, or size of the blocks *)
let input_res = ref 5.

(** [draw_slider curr_res] draws the slider ui with the width specified by
    [curr_res] *)
let draw_slider curr_res =
  draw_text "RESOLUTION" 35 170 15 Color.blue;
  draw_rectangle 30 200 curr_res 20 Color.blue;
  draw_rectangle 25 195 5 30 Color.black;
  draw_rectangle 25 195 115 5 Color.black;
  draw_rectangle 135 195 5 30 Color.black;
  draw_rectangle 25 220 115 5 Color.black

let in_slider_range x_pos y_pos =
  x_pos >= 30 && x_pos <= 130 && y_pos >= 200 && y_pos <= 220

(** [draw_ui seed] creates the user interface to display basic functions and the
    current [seed] the perlin noise is based on *)
let draw_ui seed res =
  draw_rectangle 10 10 150 250 Color.white;
  draw_text ("Current Seed: " ^ string_of_int seed) 45 50 10 Color.black;
  draw_text ("Input Seed: " ^ !input_seed) 45 60 10 Color.black;
  draw_text "Press R To" 57 80 10 Color.black;
  draw_text "Change Current Seed" 35 90 10 Color.black;
  draw_text "Drag Mouse To" 46 120 10 Color.black;
  draw_text "Move Camera" 50 130 10 Color.black;
  draw_slider (int_of_float res);
  end_drawing ()

(** [new_mat seed] creates a new perlin noise matrix utilizing a random table
    created from the inputted [seed] *)
let new_mat seed =
  grid_fbm 0 0 600 6 convert_grayscale (RTG.gen_random_table seed)

(** [loop mat seed camera] is the main game loop which repeatedly draws the
    [mat] created by the specified [seed] projected onto 3D space by the
    [camera] *)
let rec loop mat seed camera =
  let open Raylib in
  if window_should_close () then close_window ()
  else if
    is_mouse_button_down MouseButton.Left
    && in_slider_range (get_mouse_x ()) (get_mouse_y ())
  then (
    (* Slider Logic *)
    input_res := float_of_int (get_mouse_x () - 25);
    draw_perlin camera mat !input_res;
    draw_ui seed !input_res;
    loop mat seed camera)
  else if is_mouse_button_down MouseButton.Left then (
    (* Camera Movement Logic *)
    update_camera (addr camera) CameraMode.Third_person;
    draw_perlin camera mat !input_res;
    draw_ui seed !input_res;
    loop mat seed camera)
  else if is_key_pressed Key.R then (
    let new_seed = Raylib.get_random_value 0 1000 in
    draw_perlin camera mat !input_res;
    draw_ui new_seed !input_res;
    loop (new_mat new_seed) new_seed camera)
  else if is_key_pressed Key.Enter then (
    (* Makes the inputted seed display on screen if valid integer *)
    match int_of_string !input_seed with
    | s ->
        input_seed := "";
        draw_perlin camera mat !input_res;
        draw_ui seed !input_res;
        loop (new_mat s) s camera
    | exception _ ->
        input_seed := "";
        draw_perlin camera mat !input_res;
        draw_ui seed !input_res;
        loop mat seed camera)
  else (
    (* Allows for the user to input a seed, just keeps checking iteratively *)
    update_seed_input ();
    draw_perlin camera mat !input_res;
    draw_ui seed !input_res;
    loop mat seed camera)

let () = cam_setup () |> loop (new_mat 5) 5
