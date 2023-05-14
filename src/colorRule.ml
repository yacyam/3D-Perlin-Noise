open Raylib.Color

let wrap n = if n < 0 then 0 else if n > 255 then 255 else n

let rule_grayscale x =
  let x = wrap x in
  create x x x 255

let rule_bluegreenscale x =
  let x = wrap x in
  create 0 x (255 - x) 255

let color_gray x =
  let color = Int.abs (int_of_float ((x +. 0.95) /. 2. *. 255.0)) in
  color |> rule_grayscale

let rule_landscape x =
  let x = wrap x in
  if x < 100 then
    create 0 0
      (40 + int_of_float (Float.round (float_of_int x *. (215. /. 100.))))
      255
  else if x > 110 then
    create
      (int_of_float
         (10. -. Float.round (float_of_int (x - 110) *. (10. /. 145.))))
      (int_of_float
         (Float.round
            (160. -. (float_of_int (x - 110) *. (160. /. 145.)) +. 50.)))
      (int_of_float
         (60. -. Float.round (float_of_int (x - 110) *. (60. /. 145.))))
      255
  else
    let r, g, b =
      ( Int.abs
          (int_of_float (((float_of_int x -. 100.) *. (60. /. 10.)) +. 180.)),
        Int.abs
          (int_of_float (((float_of_int x -. 100.) *. (40. /. 10.)) +. 188.)),
        Int.abs
          (int_of_float (((float_of_int x -. 100.) *. (20. /. 10.)) +. 153.)) )
    in
    create r g b 255

let rule_rust x =
  let x = wrap x in
  let level = int_of_float (float_of_int x /. 255. *. 10.) mod 5 in
  if level = 0 then create 183 65 14 255
  else if level = 1 || level = 4 then
    let r, g, b =
      ( Int.abs (int_of_float (float_of_int x *. 183. /. 255.)),
        Int.abs (int_of_float (float_of_int x *. 65. /. 255.)),
        Int.abs (int_of_float (float_of_int x *. 14. /. 255.)) )
    in
    create r g b 255
  else
    let r, g, b =
      ( Int.abs (int_of_float (float_of_int x *. 170. /. 300.)),
        Int.abs (int_of_float (float_of_int x *. 169. /. 300.)),
        Int.abs (int_of_float (float_of_int x *. 173. /. 300.)) )
    in
    create r g b 255

let rule_wood x =
  let x = wrap x in
  if int_of_float (float_of_int x /. 255. *. 50.) mod 5 = 0 then
    create 166 99 64 255
  else
    let r, g, b =
      ( Int.abs (int_of_float (float_of_int x *. 166. /. 255.)),
        Int.abs (int_of_float (float_of_int x *. 99. /. 255.)),
        Int.abs (int_of_float (float_of_int x *. 64. /. 255.)) )
    in
    create r g b 255
