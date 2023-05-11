open OUnit2
open Linearalg
open Matrix
open Vector
open Main

(** [get_x_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_x input]. *)
let get_x_test (name : string) (input : vector) (expected_output : float) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (get_x input) ~printer:string_of_float

(** [get_y_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_y input]. *)
let get_y_test (name : string) (input : vector) (expected_output : float) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (get_y input) ~printer:string_of_float

(** [get_z_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [get_z input]. *)
let get_z_test (name : string) (input : vector) (expected_output : float) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (get_z input) ~printer:string_of_float

(** [dot_test name (input1, input2) expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [dot input1 input2]. *)
let dot_test (name : string) ((input1, input2) : vector * vector)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (dot input1 input2) ~printer:string_of_float

let cross_test (name : string) ((input1, input2) : vector * vector)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Vector.to_string (cross input1 input2))

(** [magnitude_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [magnitude input]. *)
let magnitude_test (name : string) (input : vector) (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (magnitude input) ~printer:string_of_float

(** [norm_magnitude_test name input] constructs an OUnit test named [name] that
    asserts the property that [norm input] is a unit vector holds. *)
let norm_magnitude_test (name : string) (input : vector) : test =
  name >:: fun _ ->
  assert_equal 1.
    (Float.round (norm input |> magnitude) *. 100000. /. 100000.)
    ~printer:string_of_float

(** [norm_angle_test name input] constructs an OUnit test named [name] that
    asserts the property that [norm input] is parallel to [input] holds. *)
let norm_angle_test (name : string) (input : vector) : test =
  name >:: fun _ ->
  assert_equal 0.
    (Float.round (norm input |> cross input |> magnitude) *. 100000. /. 100000.)
    ~printer:string_of_float

(*(** [angle_test name (input1, input2) expected_output] constructs an OUnit
  test named [name] that asserts the quality of [expected_output] with [angle
  input1 input2]. *) let angle_test (name : string) ((input1, input2) : vector *
  vector) (expected_output : float) : test = name >:: fun _ -> assert_equal
  expected_output (angle input1 input2) ~printer:string_of_float*)

let get_x_tests =
  [
    get_x_test "[x] zero" (0., -10., -100.) 0.;
    get_x_test "[x] negative" (-1., -2., -2.) (-1.);
    get_x_test "[x] positive" (1., 2., 2.) 1.;
    get_x_test "[x] min_float" (Float.min_float, 1., 2.) Float.min_float;
    get_x_test "[x] max_float" (Float.max_float, -1., -2.) Float.max_float;
    get_x_test "[x] all min"
      (Float.min_float, Float.min_float, Float.min_float)
      Float.min_float;
    get_x_test "[x] all max"
      (Float.max_float, Float.max_float, Float.max_float)
      Float.max_float;
    get_x_test "[x] all zero" (0., 0., 0.) 0.;
    get_x_test "[x] pos and neg" (-1., 3., 4.) ~-.1.;
  ]

let get_y_tests =
  [
    get_y_test "[y] zero" (-10., 0., -100.) 0.;
    get_y_test "[y] negative" (-2., -1., -2.) (-1.);
    get_y_test "[y] positive" (2., 1., 2.) 1.;
    get_y_test "[y] min_float" (1., Float.min_float, 2.) Float.min_float;
    get_y_test "[y] max_float" (-1., Float.max_float, -2.) Float.max_float;
    get_y_test "[y] pos and neg" (Float.max_float, -13., 4.) ~-.13.;
    get_y_test "y only extreme" (1., -13423.24, 1.2) ~-.13423.24;
  ]

let get_z_tests =
  [
    get_z_test "[z] zero" (-10., -100., 0.) 0.;
    get_z_test "[z] negative" (-2., -2., -1.) (-1.);
    get_z_test "[z] positive" (2., 2., 1.) 1.;
    get_z_test "[z] min_float" (1., 2., Float.min_float) Float.min_float;
    get_z_test "[z] max_float" (-1., -2., Float.max_float) Float.max_float;
    get_z_test "[z] all max"
      (Float.max_float, Float.max_float, Float.max_float)
      Float.max_float;
    get_z_test "[z] very tiny nums"
      (0.00023021, 0.2321202, 0.00000000042)
      0.00000000042;
  ]

let dot_tests =
  [
    dot_test "zero with zero" ((0., 0., 0.), (0., 0., 0.)) 0.;
    dot_test "negative with positive" ((-10., -10., -1.), (9., 12., 2.)) (-212.);
    dot_test "negative with negative" ((-10., -10., -1.), (-9., -12., -2.)) 212.;
    dot_test "positive with positive"
      ((10., 20., 30.), (1. +. sqrt 2., 1., (1. +. sqrt 3.) /. 2.))
      (10. +. (10. *. sqrt 2.) +. 20. +. 15. +. (15. *. sqrt 3.));
    dot_test "mixed negative and positive"
      ((-1., 2., 3.), (-3., 1., -0.25))
      4.25;
    dot_test "orthogonal dots is 0" ((1., 1., 0.), (1., -1., 0.)) 0.;
    dot_test "unit dot unit is 1" ((1., 0., 0.), (1., 0., 0.)) 1.;
    dot_test "dot zero and max floats is 0"
      ((0., 0., 0.), (Float.max_float, Float.max_float, Float.max_float))
      0.;
    dot_test "dot zero and min floats is 0"
      ((0., 0., 0.), (Float.min_float, Float.min_float, Float.min_float))
      0.;
  ]

let cross_tests =
  [
    cross_test "cross product of zero vector and non-zero"
      ((0., 0., 0.), (1., 2., 3.))
      "(0., 0., 0.)";
    cross_test "cross product of two zero vectors"
      ((0., 0., 0.), (0., 0., 0.))
      "(0., 0., 0.)";
    cross_test "cross product of same positive vector"
      ((3.5, 1.2, 1.), (3.5, 1.2, 1.))
      "(0., 0., 0.)";
    cross_test "cross product of same negative vector"
      ((-2., -1., -5.9), (-2., -1., -5.9))
      "(0., 0., 0.)";
    cross_test "cross product of two orthogonal vectors"
      ((1., 0., 0.), (0., 1., 0.))
      "(0., 0., 1.)";
    cross_test "cross product of two vectors"
      ((-10., 0., 3.), (3.8, -9., 1.))
      "(27., 21.4, 90.)";
  ]

let magnitude_tests =
  [
    magnitude_test "zero" (0., 0., 0.) 0.;
    magnitude_test "positive" (2., 1., 0.5) (sqrt 5.25);
    magnitude_test "negative" (-3., -4., -5.) (5. *. sqrt 2.);
    magnitude_test "irrationals" (sqrt 2., 1., sqrt 3. /. 2.) (sqrt 3.75);
    magnitude_test "mixed positive and negative" (-8.1, 1.3, -2.5) (sqrt 73.55);
    magnitude_test "max float mag" (Float.max_float, 0., 0.) infinity;
    magnitude_test "min float mag" (Float.min_float, 0., 0.) 0.;
  ]

let norm_tests =
  [
    norm_magnitude_test "unit property of norm" (1., 2., 0.);
    norm_angle_test "norm preserves angle" (1., 2., 0.);
    norm_magnitude_test "unit property of norm" (2.1, -122., 0.52);
    norm_angle_test "norm preserves angle" (2.1, -122., 0.52);
    norm_magnitude_test "norm of large floats" (1324321431., 3., 12.);
    norm_magnitude_test "norm of min floats"
      (Float.min_float, 3., Float.min_float);
    norm_magnitude_test "angle of min floats preserved"
      (Float.min_float, 3., Float.min_float);
  ]

(*let angle_tests = [ angle_test "zero" ((0., 0., 0.), (0., 0., 0.)) 0.;
  angle_test "positive" ((2., 1., 0.5), (1., 3., 2.)) 0.; angle_test "negative"
  ((), ()) 0.; angle_test "irrationals" ((), ()) 0.; angle_test "mixed positive
  and negative" (((), ()) 0.; ]*)

let vector_tests =
  List.flatten
    [
      get_x_tests;
      get_y_tests;
      get_z_tests;
      dot_tests;
      cross_tests;
      magnitude_tests;
      norm_tests;
    ]

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

let to_string_elems mat_row printer =
  List.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ printer elem else acc ^ ";" ^ printer elem)
    "[" mat_row
  ^ "]"

let to_string_list printer mat =
  List.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ to_string_elems elem printer
      else acc ^ ";" ^ to_string_elems elem printer)
    "[" mat
  ^ "]"

let basic_matrix_test (name : string) (rows : int) (cols : int) (entry : 'a)
    (expected_output : 'a list list) =
  name >:: fun _ ->
  assert_equal expected_output (basic_matrix rows cols entry |> to_list)

let basic_matrix_tests =
  [
    basic_matrix_test "0 columns and rows is just the empty matrix" 0 0 0 [];
    basic_matrix_test "1 column and row matrix with integers" 1 1 1 [ [ 1 ] ];
    basic_matrix_test "10 column and row matrix with strings" 10 10 ""
      [
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
        [ ""; ""; ""; ""; ""; ""; ""; ""; ""; "" ];
      ];
    basic_matrix_test "3 rows and 5 columns matrix with booleans" 3 5 true
      [
        [ true; true; true; true; true ];
        [ true; true; true; true; true ];
        [ true; true; true; true; true ];
      ];
    basic_matrix_test "5 rows and columns matrix with basic vectors" 5 5
      (0., 0., 0.)
      [
        [ (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.) ];
        [ (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.) ];
        [ (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.) ];
        [ (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.) ];
        [ (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.); (0., 0., 0.) ];
      ];
    (* Tests for making color matrices *)
    basic_matrix_test "3 row col of red val color matrix" 3 3
      (Raylib.Color.r (Raylib.Color.create 255 255 255 255))
      (let white = Raylib.Color.r (Raylib.Color.create 255 255 255 255) in
       [
         [ white; white; white ];
         [ white; white; white ];
         [ white; white; white ];
       ]);
    basic_matrix_test "2 row col of green val color matrix" 2 2
      (Raylib.Color.g (Raylib.Color.create 120 120 120 255))
      (let white = Raylib.Color.g (Raylib.Color.create 120 120 120 255) in
       [ [ white; white ]; [ white; white ] ]);
    basic_matrix_test "2 row 3 col of blue val color matrix" 2 3
      (Raylib.Color.b (Raylib.Color.create 12 130 220 255))
      (let white = Raylib.Color.b (Raylib.Color.create 12 130 220 255) in
       [ [ white; white; white ]; [ white; white; white ] ]);
  ]

let basic_5b5_mat () = basic_matrix 5 5 0
let basic_5b5_2 () = add_entry 0 0 10 (basic_5b5_mat ())
let basic_5b5_3 () = add_entry 4 4 10 (basic_5b5_2 ())
let basic_5b5_4 () = add_entry 4 0 10 (basic_5b5_3 ())
let basic_5b5_5 () = add_entry 0 4 10 (basic_5b5_4 ())
let basic_5b5_6 () = add_entry 2 2 10 (basic_5b5_5 ())
let skinny_matrix () = basic_matrix 10 1 (0., 0., 0.)
let wide_matrix () = basic_matrix 1 100 ""
let basic_1b5 () = basic_matrix 1 5 0
let added_2b5 () = add_row [ 1; 3; 5; 7; 3 ] (basic_1b5 ())

let add_entry_test (name : string) (rows : int) (cols : int) (entry : 'a)
    (matrix : unit -> int Matrix.t) (expected_output : int list list) =
  name >:: fun _ ->
  assert_equal expected_output
    (add_entry rows cols entry (matrix ()) |> to_list)
    ~printer:(to_string_list string_of_int)

let little_wide () = basic_matrix 1 10 0

let add_entry_tests =
  [
    add_entry_test "add 10 to upper left 5x5 integer matrix" 0 0 10
      basic_5b5_mat
      [
        [ 10; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
      ];
    add_entry_test "add 10 to bottom right 5x5 integer matrix" 4 4 10
      basic_5b5_2
      [
        [ 10; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 10 ];
      ];
    add_entry_test "add 10 to bottom left 5x5 integer matrix" 4 0 10 basic_5b5_3
      [
        [ 10; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 10; 0; 0; 0; 10 ];
      ];
    add_entry_test "add 10 to top right 5x5 integer matrix" 0 4 10 basic_5b5_4
      [
        [ 10; 0; 0; 0; 10 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 10; 0; 0; 0; 10 ];
      ];
    add_entry_test "add 10 to right-most column 5x5 integer matrix" 2 4 10
      basic_5b5_4
      [
        [ 10; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 10 ];
        [ 0; 0; 0; 0; 0 ];
        [ 10; 0; 0; 0; 10 ];
      ];
    add_entry_test "add 10 to middle 5x5 integer matrix" 2 2 10 basic_5b5_5
      [
        [ 10; 0; 0; 0; 10 ];
        [ 0; 0; 0; 0; 0 ];
        [ 0; 0; 10; 0; 0 ];
        [ 0; 0; 0; 0; 0 ];
        [ 10; 0; 0; 0; 10 ];
      ];
    add_entry_test "add 10 to beginning litte_wide mat" 0 0 10 little_wide
      [ [ 10; 0; 0; 0; 0; 0; 0; 0; 0; 0 ] ];
    add_entry_test "add 10 to end litte_wide mat" 0 9 10 little_wide
      [ [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 10 ] ];
    add_entry_test "add 10 to middle litte_wide mat" 0 4 10 little_wide
      [ [ 0; 0; 0; 0; 10; 0; 0; 0; 0; 0 ] ];
  ]

let get_row_test (name : string) (row : int) (matrix : 'a Matrix.t)
    (expected_output : 'a list) =
  name >:: fun _ -> assert_equal expected_output (get_row row matrix)

(* Helpful to test out large matrices for perlin alg *)
let huge_matrix = basic_matrix 1000 1000 1
let very_skinny = basic_matrix 10000 1 0

let get_row_tests =
  [
    get_row_test "1st row of basic 5x5 matrix is all 0's" 0 (basic_5b5_mat ())
      [ 0; 0; 0; 0; 0 ];
    get_row_test "5th row of altered 5x5 matrix has 10 in bottom left/right" 4
      (basic_5b5_5 ()) [ 10; 0; 0; 0; 10 ];
    get_row_test "3rd row of altered 5x5 matrix has 10 in middle" 2
      (basic_5b5_6 ()) [ 0; 0; 10; 0; 0 ];
    get_row_test "2nd row of altered 5x5 matrix has 0 in all" 1 (basic_5b5_6 ())
      [ 0; 0; 0; 0; 0 ];
    get_row_test "4th row of altered 5x5 matrix has 0 in all" 3 (basic_5b5_6 ())
      [ 0; 0; 0; 0; 0 ];
    get_row_test "10th row of skinny matrix is a single element" 9
      (skinny_matrix ())
      [ (0., 0., 0.) ];
    get_row_test "1st row of skinny matrix is a single element" 0
      (skinny_matrix ())
      [ (0., 0., 0.) ];
    get_row_test "1st row of very skinny matrix is single element" 0 very_skinny
      [ 0 ];
    get_row_test "10000th row of very skinny matrix is single element" 9999
      very_skinny [ 0 ];
  ]

let add_row_test (name : string) (row_list : 'a list) (matrix : 'a Matrix.t)
    (expected_output : 'a list list) =
  name >:: fun _ ->
  assert_equal expected_output (add_row row_list matrix |> to_list)

let add_row_tests =
  [
    add_row_test "adding row to 1x5 matrix makes it 2x5" [ 1; 3; 5; 7; 3 ]
      (basic_1b5 ())
      [ [ 0; 0; 0; 0; 0 ]; [ 1; 3; 5; 7; 3 ] ];
    add_row_test "adding row to 2x5 matrix makes it 3x5"
      [ max_int; max_int; min_int; 0; 5 ]
      (added_2b5 ())
      [
        [ 0; 0; 0; 0; 0 ];
        [ 1; 3; 5; 7; 3 ];
        [ max_int; max_int; min_int; 0; 5 ];
      ];
    add_row_test "adding row to empty matrix makes it 1xn of any elem"
      [ ""; ""; "" ] empty
      [ [ ""; ""; "" ] ];
    add_row_test "adding row containing row to matrix makes it 1xn of row"
      [ [ ""; ""; "" ]; [ ""; ""; ""; "" ] ]
      empty
      [ [ [ ""; ""; "" ]; [ ""; ""; ""; "" ] ] ];
    add_row_test "adding row to empty matrix makes it 1xn of any elem (vector)"
      [
        (0., 3., 1.4);
        (2.1, 4.9, 2.4);
        (8.31, 4.27, 1.9);
        (91.4, 33.8, 12.4);
        (0.53, 3.23, 11.3);
      ]
      empty
      [
        [
          (0., 3., 1.4);
          (2.1, 4.9, 2.4);
          (8.31, 4.27, 1.9);
          (91.4, 33.8, 12.4);
          (0.53, 3.23, 11.3);
        ];
      ];
    (let white = Raylib.Color.b (Raylib.Color.create 12 130 220 255) in
     add_row_test "add color row to empty makes it 1xn" [ white; white; white ]
       empty
       [ [ white; white; white ] ]);
    add_row_test "add single elem to matrix makes it 1x1" [ 1 ] empty [ [ 1 ] ];
    add_row_test "add list of elems as row makes row of rows"
      [ [ 1; 3; 5; 7; 9 ]; [ 2; 4; 6; 8; 10 ] ]
      empty
      [ [ [ 1; 3; 5; 7; 9 ]; [ 2; 4; 6; 8; 10 ] ] ];
  ]

let row_length_test (name : string) (matrix : 'a Matrix.t)
    (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output (row_length matrix)

let row_length_tests =
  [
    row_length_test "empty matrix should be of length 0" empty 0;
    row_length_test "basic 5x5 matrix should be length 5" (basic_5b5_mat ()) 5;
    row_length_test "altered 5x5 matrix should still be length 5"
      (basic_5b5_6 ()) 5;
    row_length_test "skinny matrix should be length 10" (skinny_matrix ()) 10;
    row_length_test "wide matrix should be length 1" (wide_matrix ()) 1;
    row_length_test "very skinny matrix should be length 10000" very_skinny
      10000;
    row_length_test "basic 5x5 with added elements should stay length 5"
      (basic_5b5_6 ()) 5;
    row_length_test "huge matrix should have length 1000" huge_matrix 1000;
  ]

let get_entry_test (name : string) (row : int) (col : int)
    (matrix : 'a Matrix.t) (expected_output : 'a) =
  name >:: fun _ -> assert_equal expected_output (get_entry row col matrix)

let get_entry_tests =
  [
    get_entry_test "bottom right element of basic 5x5 is 0" 4 4
      (basic_5b5_mat ()) 0;
    get_entry_test "top left element of basic 5x5 is 0" 0 0 (basic_5b5_mat ()) 0;
    get_entry_test "middle element of added 5x5 is 10" 2 2 (basic_5b5_6 ()) 10;
    get_entry_test "bottom left element of added 5x5 is 10" 4 0 (basic_5b5_6 ())
      10;
    get_entry_test "top right element of added 5x5 is 10" 0 4 (basic_5b5_6 ())
      10;
    get_entry_test "50th element of first row wide matrix is (empty string)" 0
      49 (wide_matrix ()) "";
    get_entry_test "1st element of first row wide matrix is (empty string)" 0 0
      (wide_matrix ()) "";
    get_entry_test "100th element of first row wide matrix is (empty string)" 0
      99 (wide_matrix ()) "";
    get_entry_test "1st element of first row wide matrix is (empty string)" 0 0
      (wide_matrix ()) "";
    get_entry_test "7th row of skinny matrix is zero vector" 6 0
      (skinny_matrix ()) (0., 0., 0.);
    get_entry_test "10th row of skinny matrix is zero vector" 9 0
      (skinny_matrix ()) (0., 0., 0.);
    get_entry_test "1st row of skinny matrix is zero vector" 0 0
      (skinny_matrix ()) (0., 0., 0.);
    get_entry_test "10000th row 1st col of very skinny matrix is zero " 9999 0
      very_skinny 0;
    get_entry_test "1st row 1st col of very skinny matrix is zero " 0 0
      very_skinny 0;
    get_entry_test "2nd row 4th col of added 2b5 mat is 7" 1 3 (added_2b5 ()) 7;
    get_entry_test "1st elem of 1st row col of huge matrix is 1" 0 0 huge_matrix
      1;
    get_entry_test "1000th row 1000th column of huge matrix is 1" 999 999
      huge_matrix 1;
    get_entry_test "1000th row 1st column of huge matrix is 1" 999 0 huge_matrix
      1;
    get_entry_test "1st row 1000th column of huge matrix is 1" 0 999 huge_matrix
      1;
    get_entry_test "500th row 500th column of huge matrix is 1" 499 499
      huge_matrix 1;
    get_entry_test "250th row 250th column of huge matrix is 1" 249 249
      huge_matrix 1;
    get_entry_test "750th row 750th column of huge matrix is 1" 749 249
      huge_matrix 1;
  ]

let matrix_tests =
  List.flatten
    [
      basic_matrix_tests;
      add_entry_tests;
      get_row_tests;
      add_row_tests;
      row_length_tests;
      get_entry_tests;
    ]

let dot_grad_dist_test (name : string) (random : int) (distance_vector : vector)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Float.round (dot_grad_dist random distance_vector *. 100.) /. 100.)
    ~printer:string_of_float

let dot_grad_dist_tests =
  [
    dot_grad_dist_test "not_rotated" 0 (2., 5., 0.) 7.;
    dot_grad_dist_test "rotated pi/2" 1 (-10.2, -4., 0.) 6.2;
    dot_grad_dist_test "rotated pi" 3 (-9.2, 2., 0.) 7.2;
    dot_grad_dist_test "rotated 3pi/2" 2 (1.3, -2., 0.) 3.3;
    dot_grad_dist_test "rand val bounded" 1243123 (12., 13., 0.) ~-.25.;
    dot_grad_dist_test "neg val bounded" ~-123 (2., 4., 0.) ~-.6.;
  ]

let interpolate_test (name : string) (u_l : float) (u_r : float) (l_l : float)
    (l_r : float) (frac_x : float) (frac_y : float) (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (Float.round (interpolate u_l u_r l_l l_r frac_x frac_y *. 100.) /. 100.)
    ~printer:string_of_float

let interpolate_tests =
  [
    interpolate_test "0.5 fracs" 7. 1. 2. (-3.) 0.5 0.5 1.75;
    interpolate_test "0.4 frac 0.1 frac" 6.2 1.2 0.3 1.1 0.4 0.1 0.59;
    interpolate_test "1.0 fracs" 7.2 (-3.3) (-4.0) (-2.1) 1.0 1.0 (-3.3);
    interpolate_test "0. fracs" 3.3 4.4 5.5 6.6 0. 0. 5.5;
  ]

(* size of actual grid for perlin alg *)
let basic_mat = basic_dist_mat 600

let basic_dist_mat_tests =
  [
    get_entry_test "first entry basic dist is 0 vector" 0 0 basic_mat
      (0., 0., 0.);
    get_entry_test "last entry basic dist is 0 vector" 599 599 basic_mat
      (0., 0., 0.);
    get_entry_test "middle elem basic dist is 0 vector" 299 299 basic_mat
      (0., 0., 0.);
    row_length_test "size of rows are 600" basic_mat 600;
  ]

let basic_smaller_mat () = basic_dist_mat 200
let dist_mat = distance_matrix (basic_smaller_mat ()) 200 0 0
let weird_dist = distance_matrix (basic_smaller_mat ()) 200 1000 1000
let diff_x_y_mat = distance_matrix (basic_smaller_mat ()) 200 20 1300

(** [norm_rev (x, y, z)] reverses the normalized vector and rounds so equality
    can be asserted on integers (floats can't be asserted correctly) *)
let norm_rev (x, y, z) =
  ( int_of_float (x *. sqrt 2.),
    int_of_float (y *. sqrt 2.),
    int_of_float (z *. sqrt 2.) )

let normalize_dist_test (name : string) (vec : float * float * float)
    expected_output =
  name >:: fun _ -> assert_equal expected_output (normalize_dist vec |> norm_rev)

let normalize_dist_tests =
  [
    normalize_dist_test "normalized dist when rev should be dist 0 vec"
      (0., 0., 0.) (0, 0, 0);
    normalize_dist_test "normalized dist when rev should be dist regular vec"
      (13., 24., 92.) (13, 24, 92);
    normalize_dist_test "normalized dist when rev should be dist extreme vec"
      (99999999., 99999999., 0.) (99999999, 99999999, 0);
  ]

let dist_entry_test (name : string) (y : int) (x : int) (mat : vector t)
    (expected_output : int * int * int) =
  name >:: fun _ -> assert_equal expected_output (get_entry y x mat |> norm_rev)

let distance_matrix_tests =
  [
    dist_entry_test "first entry must be of distance 0" 0 0 dist_mat (0, 0, 0);
    dist_entry_test "last entry must be of distance 199" 199 199 dist_mat
      (199, 199, 0);
    dist_entry_test "top left entry must have y dist 199" 199 0 dist_mat
      (0, 199, 0);
    dist_entry_test "bottom right entry must have x dist 199" 0 199 dist_mat
      (199, 0, 0);
    dist_entry_test "middle entry must have x and y dist 99" 99 99 dist_mat
      (99, 99, 0);
    dist_entry_test "regular entry must have diff x and y dist" 34 102 dist_mat
      (102, 34, 0);
    dist_entry_test "another regular entry must have diff x and y dist" 154 2
      dist_mat (2, 154, 0);
    dist_entry_test "first entry weird must start with 1000" 0 0 weird_dist
      (1000, 1000, 0);
    dist_entry_test "last entry weird must end with 1199" 199 199 weird_dist
      (1199, 1199, 0);
    dist_entry_test "top left entry weird y dist 1199 and x 1000" 199 0
      weird_dist (1000, 1199, 0);
    dist_entry_test "top left entry weird y dist 1000 x 1199" 0 199 weird_dist
      (1199, 1000, 0);
    dist_entry_test "middle entry weird y dist 1099 x 1099" 99 99 weird_dist
      (1099, 1099, 0);
    dist_entry_test "regular entry weird must have diff x and y dist" 174 22
      weird_dist (1022, 1174, 0);
    dist_entry_test "another regular entry weird must have diff x and y dist"
      112 122 weird_dist (1122, 1112, 0);
    dist_entry_test "first entry is diff x and y start w/ x < y" 0 0
      diff_x_y_mat (20, 1300, 0);
    dist_entry_test "last entry is diff x and y end w/ x < y" 199 199
      diff_x_y_mat (219, 1499, 0);
    dist_entry_test "top left is diff x and y w/ x at start" 199 0 diff_x_y_mat
      (20, 1499, 0);
    dist_entry_test "bottom right is diff x and y w/ y at start" 0 199
      diff_x_y_mat (219, 1300, 0);
    dist_entry_test "middle elem is diff x and y at center" 99 99 diff_x_y_mat
      (119, 1399, 0);
    dist_entry_test "regular entry is diff x and y w/ x < y" 115 93 diff_x_y_mat
      (113, 1415, 0);
    dist_entry_test "another regular entry is diff x and y w/ x < y" 9 148
      diff_x_y_mat (168, 1309, 0);
  ]

let main_tests =
  List.flatten
    [
      dot_grad_dist_tests;
      interpolate_tests;
      basic_dist_mat_tests;
      distance_matrix_tests;
      normalize_dist_tests;
    ]

let suite =
  "test suite for project"
  >::: List.flatten [ vector_tests; matrix_tests; main_tests ]

let _ = run_test_tt_main suite
