open Raylib
open Linearalg
open Vector
open ColorRule

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

(** [pixel_mat_fbm rgb_mat d_mat x y size n_octaves colorize rand_vals] returns
    a matrix [rgb_mat] that's the Main.ml noise function applied to the distance
    vectors in each entry of [d_mat] *)
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

(** [grid_fbm x y size n_octaves colorize rand_vals] is a color matrix of
    dimensions [size] by [size] with entries starting from the [x] and [y]
    positions. *)
let rec grid_fbm x y size n_octaves colorize rand_vals =
  let dmat = Main.distance_matrix (Main.basic_dist_mat size) size x y in
  pixel_mat_fbm (gray_matrix size) dmat 0 0 size n_octaves colorize rand_vals

(** [cam_setup ()] is the starting static camera position/angle *)
let cam_setup () =
  let open Raylib in
  init_window 1000 600 "3D Perlin Noise";
  let camera =
    Camera.create
      (Vector3.create 350.0 200.0 620.0) (* position *)
      (Vector3.create 300.0 300.0 0.0) (* target *)
      (Vector3.create 0.0 1.0 50.0) (* up *)
      90.0 (* FOV *) CameraProjection.Perspective
  in
  set_target_fps 60;
  camera

let color_func (rule : int -> Color.t) (color : Color.t) : Color.t =
  let gray = Color.g color in
  rule gray

type pause =
  | Pause
  | Resume

type change =
  | Plus of pause
  | Minus of pause

let over_time = ref (0, Plus Pause)

type initial_condition =
  | Known of Color.t Linearalg.Matrix.t
  | Unknown

type blur_data = {
  mutable t : float;
  mutable control : pause;
  mutable init : initial_condition;
}

let blur_timer = { t = 0.; control = Pause; init = Unknown }

(** [draw_perlin camera mat res color_rule] draws the perlin noise on a
    three-dimensional axis based on the colors specified by [mat] transformed by
    [color_rule] and the starting [camera] position with the specified
    resolution [res] *)
let draw_perlin camera mat res color_rule =
  begin_drawing ();
  clear_background Color.skyblue;
  begin_mode_3d camera;
  let () =
    match blur_timer.init with
    | Known ic -> ()
    | Unknown -> blur_timer.init <- Known mat
  in
  let mat_x = Matrix.row_length mat |> float_of_int in
  let rec draw_seq x y =
    if y >= mat_x then ()
    else if x >= mat_x then draw_seq 0. (y +. res)
    else
      let mat_entry = Matrix.get_entry (int_of_float x) (int_of_float y) mat in
      (* takes specified perlin value from !over_time then normalizes it so it
         scales perlin ever so slightly over time *)
      let entry_scaler =
        Matrix.get_entry (int_of_float x) (fst !over_time) mat
      in
      let norm_scaler =
        1.0 +. ((Color.g entry_scaler |> float_of_int) /. 255.)
      in
      let neighbor_contribution =
        let left =
          if int_of_float y = 0 then mat_entry
          else Matrix.get_entry (int_of_float x) (int_of_float y - 1) mat
        in
        let right =
          if int_of_float y = int_of_float mat_x then mat_entry
          else Matrix.get_entry (int_of_float x) (int_of_float y + 1) mat
        in
        let top =
          if int_of_float x = 0 then mat_entry
          else Matrix.get_entry (int_of_float x - 1) (int_of_float y) mat
        in
        let bottom =
          if int_of_float x = int_of_float mat_x then mat_entry
          else Matrix.get_entry (int_of_float x + 1) (int_of_float y) mat
        in
        (-4 * Color.g mat_entry)
        + Color.g left + Color.g right + Color.g top + Color.g bottom
      in
      let scaled_mat_entry_value =
        Color.g mat_entry
        + (neighbor_contribution * int_of_float (10. *. sin blur_timer.t))
      in
      let scaled_mat_entry_value =
        if scaled_mat_entry_value < 0 then 0
        else if scaled_mat_entry_value > 255 then 255
        else scaled_mat_entry_value
      in
      let scaled_mat_entry =
        Color.create scaled_mat_entry_value scaled_mat_entry_value
          scaled_mat_entry_value 255
      in
      draw_cube (Vector3.create x y 0.0) res res
        ((Color.g scaled_mat_entry |> float_of_int) *. norm_scaler)
        (color_func color_rule scaled_mat_entry);
      draw_seq (x +. res) y
  in
  draw_seq 0.0 0.0;
  end_mode_3d ()

(** [input_seed] is the string representation of the user inputted seed *)
let input_seed = ref ""

(** [update_seed_input ()] mutably updates the string holding the user inputted
    seed *)
let update_seed_input key =
  if String.length !input_seed < 7 then
    match key with
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

(** [not_clicked] denotes when the user has clicked on the screen. *)
let not_clicked = ref true

(** [draw_slider curr_res] draws the slider ui with the width specified by
    [curr_res] *)
let draw_slider curr_res =
  draw_text "RESOLUTION" 35 190 15 Color.blue;
  draw_rectangle 30 220 curr_res 20 Color.blue;
  draw_rectangle 25 215 5 30 Color.black;
  draw_rectangle 25 215 115 5 Color.black;
  draw_rectangle 135 215 5 30 Color.black;
  draw_rectangle 25 240 115 5 Color.black

let in_slider_range x_pos y_pos =
  x_pos >= 30 && x_pos <= 130 && y_pos >= 220 && y_pos <= 240

(** [draw_ui seed res] creates the user interface to display basic functions and
    the current [seed] the perlin noise is based on, with slider of size [res] *)
let draw_main_ui seed res =
  draw_rectangle 10 10 170 250 Color.white;
  draw_text ("Current Seed: " ^ string_of_int seed) 35 30 10 Color.black;
  draw_text ("Input Seed: " ^ !input_seed) 35 40 10 Color.black;
  draw_text "Inputs" 55 60 10 Color.black;
  draw_text "R: Random Seed" 35 70 10 Color.black;
  draw_text "A: Gray Noise" 35 80 10 Color.black;
  draw_text "S: Bluegreen Noise" 35 90 10 Color.black;
  draw_text "D: Landscape Noise" 35 100 10 Color.black;
  draw_text "F: Rust Noise" 35 110 10 Color.black;
  draw_text "P: Enter Playground" 35 120 10 Color.black;
  draw_text "W: Trigger Blur" 35 130 10 Color.black;
  draw_text "LShift: Reset Blur" 35 140 10 Color.black;
  draw_text "SPACE: Play/Pause" 35 150 10 Color.black;
  draw_text "Mutations" 80 160 10 Color.black;
  if !not_clicked then
    draw_text "* Drag Mouse To Move Camera *" 600 10 20 Color.brown;
  draw_slider (int_of_float res);
  end_drawing ()

(** [draw_play_ui ()] creates the user interface for playground mode *)
let draw_play_ui () =
  draw_rectangle 10 10 150 250 Color.white;
  draw_text "PLAYGROUND" 50 50 10 Color.black;
  draw_text "Q: Small Brush" 35 70 10 Color.black;
  draw_text "W: Medium Brush" 35 80 10 Color.black;
  draw_text "E: Large Brush" 35 90 10 Color.black;
  draw_text "R: Huge Brush" 35 100 10 Color.black;
  draw_text "P: Exit Playground" 35 110 10 Color.black;
  draw_text "Left Click: Draw" 25 150 15 Color.black;
  draw_text "Right Click: Erase" 20 165 15 Color.black;
  end_drawing ()

(** [new_mat seed] creates a new perlin noise matrix utilizing a random table
    created from the inputted [seed] *)
let new_mat seed = grid_fbm 0 0 600 6 color_gray (RTG.gen_random_table seed)

(** [change_logic ()] changes the perlin to move based on a set boundary if the
    user specified it to be resumed. If paused, nothing is changed.*)
let change_logic () =
  let bounds () =
    match snd !over_time with
    | Plus Resume ->
        if fst !over_time >= 599 then over_time := (599, Minus Resume)
        else over_time := (fst !over_time + 1, Plus Resume)
    | Minus Resume ->
        if fst !over_time <= 0 then over_time := (0, Plus Resume)
        else over_time := (fst !over_time - 1, Minus Resume)
    | _ -> ()
  in
  bounds ()

let blur_logic () =
  match blur_timer.control with
  | Resume -> blur_timer.t <- blur_timer.t +. 0.05
  | Pause -> ()

let blank_mat () = Matrix.basic_matrix 600 600 (Color.create 0 0 0 255)

let top_down_cam () =
  Camera.create
    (Vector3.create 300.0 300.0 300.0) (* position *)
    (Vector3.create 300.0 300.0 0.0) (* target *)
    (Vector3.create 0.0 1.0 50.0) (* up *)
    90.0 (* FOV *) CameraProjection.Perspective

let updated_entry mat x y height =
  let new_col = Color.g (Matrix.get_entry x y mat) + height in
  if new_col >= 250 then Color.create 250 250 250 255
  else if new_col <= 0 then Color.create 5 5 5 255
  else Color.create new_col new_col new_col 255

(** [update_multiple mat x y n n_hold] updates an [n]x[n] grid of the matrix
    [mat] with an incremented color of [height] height. *)
let rec update_multiple mat x y n height =
  (* [n_hold] holds the initial value of [n] so cubes can be created. *)
  let n_hold = n in
  let rec add_to_y mat x y n =
    if n <= 0 || y >= 600 then mat
    else
      let rec add_to_x mat x y n =
        if n <= 0 || x >= 600 then mat
        else
          let entry = updated_entry mat x y height in
          let new_mat = Matrix.add_entry x y entry mat in
          add_to_x new_mat (x + 1) y (n - 1)
      in
      let up_mat = add_to_x mat x y n_hold in
      add_to_y up_mat x (y + 1) (n - 1)
  in
  add_to_y mat x y n

(** [loop mat seed color_rule camera] is the main game loop which repeatedly
    draws the [mat] created by the specified [seed] projected onto 3D space by
    the [camera], colored by [color_rule] *)
let rec loop mat seed color_rule camera =
  let open Raylib in
  if window_should_close () then close_window ()
  else if is_mouse_button_down MouseButton.Left then
    mouse_logic mat seed color_rule camera
  else
    match get_key_pressed () with
    | Key.R -> draw_random mat color_rule camera
    | Key.A -> loop mat seed rule_grayscale camera
    | Key.S -> loop mat seed rule_bluegreenscale camera
    | Key.D -> loop mat seed rule_landscape camera
    | Key.F -> loop mat seed rule_rust camera
    | Key.P ->
        input_res := 5.;
        enter_playground (blank_mat ()) (top_down_cam ()) 30
    | Key.Enter -> draw_input mat seed color_rule camera
    | Key.Space -> play_movement mat seed color_rule camera
    | Key.W -> play_blur mat seed color_rule camera
    | Key.Left_shift ->
        blur_timer.t <- 0.;
        loop mat seed color_rule camera
    | key ->
        (* Allows for the user to input a seed, just keeps checking
           iteratively *)
        update_seed_input key;
        blur_logic ();
        change_logic ();
        draw_perlin camera mat !input_res color_rule;
        draw_main_ui seed !input_res;
        loop mat seed color_rule camera

(** [draw_random mat color_rule camera] updates the noise with random seed *)
and draw_random mat color_rule camera =
  let new_seed = Raylib.get_random_value 0 1000 in
  draw_perlin camera mat !input_res color_rule;
  draw_main_ui new_seed !input_res;
  loop (new_mat new_seed) new_seed color_rule camera

(** [draw_input mat seed color_rule camera] updates the noise with user inputted
    seed if valid. If input is empty, seed unchanged. *)
and draw_input mat seed color_rule camera =
  (* Makes the inputted seed display on screen if valid integer *)
  draw_perlin camera mat !input_res color_rule;
  draw_main_ui seed !input_res;
  match int_of_string !input_seed with
  | s ->
      input_seed := "";
      loop (new_mat s) s color_rule camera
  | exception _ ->
      input_seed := "";
      loop mat seed color_rule camera

(** [mouse_logic mat seed color_rule camera] handles the logic for dragging the
    mouse on the screen, based on it's position. *)
and mouse_logic mat seed color_rule camera =
  not_clicked := false;
  if in_slider_range (get_mouse_x ()) (get_mouse_y ()) then (
    (* Slider Logic *) input_res := float_of_int (get_mouse_x () - 25);
    draw_perlin camera mat !input_res color_rule;
    draw_main_ui seed !input_res;
    loop mat seed color_rule camera)
  else (
    (* Camera Movement Logic *)
    update_camera (addr camera) CameraMode.Third_person;
    draw_perlin camera mat !input_res color_rule;
    draw_main_ui seed !input_res;
    loop mat seed color_rule camera)

(** [play_movement mat seed color_rule camera] updates the noise to either pause
    or resume and mutate in height. *)
and play_movement mat seed color_rule camera =
  let index = fst !over_time in
  (match snd !over_time with
  | Plus Resume -> over_time := (index, Plus Pause)
  | Minus Resume -> over_time := (index, Minus Pause)
  | Plus Pause -> over_time := (index, Plus Resume)
  | Minus Pause -> over_time := (index, Minus Resume));
  loop mat seed color_rule camera

(** [play_blur mat seed color_rule camera] updates the noise to either pause or
    resume. *)
and play_blur mat seed color_rule camera =
  (match blur_timer.control with
  | Resume ->
      blur_timer.control <- Pause;
      blur_timer.init <- Known mat
  | Pause -> blur_timer.control <- Resume);
  loop mat seed color_rule camera

(** [enter_playground mat camera brush_size] creates a top-down view of a blank
    canvas where user clicks can incrementally draw noise directly on it with a
    size of [brush_size] *)
and enter_playground mat camera brush_size =
  if window_should_close () then close_window ();
  draw_perlin camera mat !input_res rule_grayscale;
  draw_play_ui ();
  match get_key_pressed () with
  | Key.P -> loop mat 0 rule_grayscale (cam_setup ())
  | Key.Q -> enter_playground mat camera 10
  | Key.W -> enter_playground mat camera 30
  | Key.E -> enter_playground mat camera 50
  | Key.R -> enter_playground mat camera 100
  | _ ->
      let x_val = get_mouse_x () - 200 in
      let y_val = 600 - get_mouse_y () in
      if x_val >= 0 && x_val <= 600 && y_val >= 0 && y_val <= 600 then
        if is_mouse_button_down MouseButton.Left then
          (* Pushes perlin up *)
          let new_mat = update_multiple mat x_val y_val brush_size 10 in
          enter_playground new_mat camera brush_size
        else if is_mouse_button_down MouseButton.Right then
          (* Pushes perlin down *)
          let new_mat = update_multiple mat x_val y_val brush_size (-10) in
          enter_playground new_mat camera brush_size
        else enter_playground mat camera brush_size
      else enter_playground mat camera brush_size

let () = cam_setup () |> loop (new_mat 5) 5 rule_grayscale
