open Matrix
open Vector

(** THIS IS UNCOMMENTED FOR FORMATTING REASONS (OCAML FORMATS THE LIST TO BE
    VERTICAL!). So uncomment this list if you are testing out code and delete
    the mock list right under it.*)
(* let random_values = [ 150; 41; 28; 219; 21; 206; 177; 13; 248; 101; 136; 210;
   250; 118; 137; 148; 71; 7; 131; 90; 82; 3; 36; 224; 103; 38; 144; 218; 2;
   148; 71; 119; 87; 13;204; 206; 136; 141; 62; 133; 142; 118; 93; 127; 80; 140;
   81; 130; 6; 138; 210;99; 239; 111; 97; 103; 231; 19; 62; 78; 148; 118; 202;
   140; 14; 78; 116; 104; 127; 252; 124; 199; 244; 70; 182; 240; 96; 193; 198;
   5; 106; 161; 242; 165; 248; 205; 173; 17; 46; 133; 106; 30; 55; 81; 81; 185;
   114; 20; 27; 99; 120; 227; 1; 250; 32; 96; 162; 113; 118; 155; 197; 213; 21;
   226; 28; 134; 108; 173; 45; 87; 15; 153; 122; 16; 187; 204; 24; 252; 20; 164;
   126; 56; 90; 25; 111; 136; 128; 61; 227; 167; 175; 74; 49; 225; 117; 27; 231;
   25; 145; 208; 237; 110; 52; 163; 25; 151; 132; 31; 214; 196; 226; 184; 224;
   23; 63; 120; 201; 46; 88; 2; 147; 83; 12; 98; 160; 169; 96; 217; 71; 114; 90;
   53; 86; 174; 212; 2; 123; 61; 13; 2; 14; 88; 236; 233; 11; 77; 136; 209; 65;
   4; 171; 80; 210; 81; 201; 184; 79; 208; 206; 60; 72; 120; 109; 222; 180; 138;
   209; 240; 103; 238; 41; 244; 233; 134; 141; 143; 187; 237; 232; 224; 139;
   222; 14; 59; 87; 31; 2; 114; 6; 83; 66; 160; 38; 55; 220; 26; 111; 176; 101;
   200; 227; 154; 29; 148; 126; 205 ] *)

(** DELETE THIS ONCE YOU UNCOMMENT LIST ABOVE.*)
let random_values = [ 2; 3; 4 ]

(** [smooth] is the result of apply the function (6n^2) - (15n^4) + (10n^3) to
    [n]. The resulting n will be used in interpolation.*)
let smooth n =
  let poly_1 = n *. n *. n *. n *. n *. 6.0 in
  let poly_2 = n *. n *. n *. n *. 15.0 in
  let poly_3 = n *. n *. n *. 10.0 in
  poly_1 -. poly_2 +. poly_3

(** [interpolate] is a linear interpolation of dot products of each corner of a
    pixel. *)
let interpolate upper_left upper_right lower_left lower_right frac_x frac_y =
  let smooth_x = smooth frac_x in
  let smooth_y = smooth frac_y in
  let interpolate_1 =
    upper_right -. upper_left |> ( *. ) smooth_x |> ( +. ) upper_left
  in
  let interpolate_2 =
    lower_right -. lower_left |> ( *. ) smooth_x |> ( +. ) lower_left
  in
  interpolate_2 -. interpolate_1 |> ( *. ) smooth_y |> ( +. ) interpolate_1

(** [dot_grad_dist] is the dot product of a [distane_vector] and a gradient
    vector that is chosen based on [random] value when called in
    [gradient_of_pixel]. *)
let dot_grad_dist (random : int) (distance_vector : vector) : float =
  if random = 0 then dot (1.0, 1.0, 0.0) distance_vector
  else if random = 1 then dot (-1.0, 1.0, 0.0) distance_vector
  else if random = 2 then dot (1.0, -1.0, 0.0) distance_vector
  else if random = 3 then dot (-1.0, -1.0, 0.0) distance_vector
  else 0.0

(** [gradient_of_pixel] is the final color value of a pixel. *)
let gradient_of_pixel (pixel_pos : vector) =
  let x_float = get_x pixel_pos in
  let y_float = get_y pixel_pos in
  let x_pos = Float.to_int x_float in
  let y_pos = Float.to_int (get_y pixel_pos) in
  let g1 = List.nth random_values x_pos in
  let g1_final = List.nth random_values (g1 + y_pos) in
  let g2 = List.nth random_values (x_pos + 1) in
  let g2_final = List.nth random_values (g2 + y_pos) in
  let g3 = List.nth random_values x_pos in
  let g3_final = List.nth random_values (g3 + y_pos + 1) in
  let g4 = List.nth random_values (x_pos + 1) in
  let g4_final = List.nth random_values (g4 + y_pos + 1) in
  let frac_x = x_float -. Float.floor x_float in
  let frac_y = y_float -. Float.floor y_float in
  let d1 = dot_grad_dist g1_final (frac_x, frac_y, 0.0) in
  let d2 = dot_grad_dist g2_final (frac_x -. 1.0, frac_y, 0.0) in
  let d3 = dot_grad_dist g3_final (frac_x, frac_y -. 1.0, 0.0) in
  let d4 = dot_grad_dist g4_final (frac_x -. 1.0, frac_y -. 1.0, 0.0) in
  interpolate d1 d2 d3 d4
