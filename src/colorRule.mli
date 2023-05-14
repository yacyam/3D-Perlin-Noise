open Raylib.Color

val rule_grayscale : int -> t
(** [convert_grayscale x] takes in a float (random number generated from perlin
    noise) and outputs a grayscale rgb value based on the range the value [x]
    encompassed. x should be in 0..255. *)

val rule_bluegreenscale : int -> t
(** [convert_bluegreenscale x] takes in a float (random number generated from
    perlin noise) and outputs a blue-green rgb scaled value based on [x]. x
    should be in 0..255. *)

val color_gray : float -> t
(** [color_gray x] takes x, a float from distance matrix and converts it to an
    int in range 0..255. However, can produce values outside of that range.
    Those are caught in other functions. Offsets by small factor to make terrain
    more realistic. *)

val rule_landscape : int -> t
(** [convert_landmass x] takes in a float (random number generated from perlin
    noise) and outputs a blue or green rgb scaled value based on [x]. x should
    be in 0..255. If x is small enough, it outputs a blue scale value, otherwise
    it's green scale. *)

val rule_rust : int -> t
(** [rule_rust x] takes in an int (random number generated from perlin noise)
    and outputs a brown rgb scaled value based on [x]. x should be in 0..255. *)

val rule_wood : int -> t
(** [rule_wood x] takes in an int (random number generated from perlin noise)
    and outputs a brown rgb scaled value based on [x]. x should be in 0..255. *)
