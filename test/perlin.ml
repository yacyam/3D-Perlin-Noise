open OUnit2
open Linearalg
open Matrix
open Vector

let suite = "test suite for project" >::: []
let _ = run_test_tt_main suite

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

(** [magnitude_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [magnitude input]. *)
let magnitude_test (name : string) (input : vector) (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (magnitude input) ~printer:string_of_float

(*(** [angle_test name (input1, input2) expected_output] constructs an OUnit
  test named [name] that asserts the quality of [expected_output] with [angle
  input1 input2]. *) let angle_test (name : string) ((input1, input2) : vector *
  vector) (expected_output : float) : test = name >:: fun _ -> assert_equal
  expected_output (angle input1 input2) ~printer:string_of_float*)

let get_x_tests =
  [
    get_x_test "zero" (0., -10., -100.) 0.;
    get_x_test "negative" (-1., -2., -2.) (-1.);
    get_x_test "positive" (1., 2., 2.) 1.;
    get_x_test "min_float" (Float.min_float, 1., 2.) Float.min_float;
    get_x_test "max_float" (Float.max_float, -1., -2.) Float.max_float;
  ]

let get_y_tests =
  [
    get_y_test "zero" (-10., 0., -100.) 0.;
    get_y_test "negative" (-2., -1., -2.) (-1.);
    get_y_test "positive" (2., 1., 2.) 1.;
    get_y_test "min_float" (1., Float.min_float, 2.) Float.min_float;
    get_y_test "max_float" (-1., Float.max_float, -2.) Float.max_float;
  ]

let get_z_tests =
  [
    get_z_test "zero" (-10., -100., 0.) 0.;
    get_z_test "negative" (-2., -2., -1.) (-1.);
    get_z_test "positive" (2., 2., 1.) 1.;
    get_z_test "min_float" (1., 2., Float.min_float) Float.min_float;
    get_z_test "max_float" (-1., -2., Float.max_float) Float.max_float;
  ]

let dot_tests =
  [
    dot_test "zero with zero" ((0., 0., 0.), (0., 0., 0.)) 0.;
    dot_test "negative with positive" ((-10., -10., -1.), (9., 12., 2.)) (-212.);
    dot_test "negative with negative" ((-10., -10., -1.), (-9., -12., -2.)) 212.;
    dot_test "positive with positive"
      ((10., 20., 30.), (1. +. sqrt 2., 1., 1. *. sqrt 3. /. 2.))
      (10. +. (10. *. sqrt 2.) +. 20. +. 15. +. (15. *. sqrt 3.));
    dot_test "mixed negative and positive"
      ((-1., 2., 3.), (-3., 1., -0.25))
      4.25;
  ]

let magnitude_tests =
  [
    magnitude_test "zero" (0., 0., 0.) 0.;
    magnitude_test "positive" (2., 1., 0.5) (sqrt 5.25);
    magnitude_test "negative" (-3., -4., -5.) (5. *. sqrt 2.);
    magnitude_test "irrationals" (sqrt 2., 1., sqrt 3. /. 2.) (sqrt 5.25);
    magnitude_test "mixed positive and negative" (-8.1, 1.3, -2.5) (sqrt 73.55);
  ]

(*let angle_tests = [ angle_test "zero" ((0., 0., 0.), (0., 0., 0.)) 0.;
  angle_test "positive" ((2., 1., 0.5), (1., 3., 2.)) 0.; angle_test "negative"
  ((), ()) 0.; angle_test "irrationals" ((), ()) 0.; angle_test "mixed positive
  and negative" (((), ()) 0.; ]*)
