open Graphics
open Linearalg

(* THIS IS JUST A TEST OF THE GRAPHICS LIBRARY *)
let _ = open_graph ""
let () = set_window_title "Basic Starter Code"
let () = resize_window 600 600
let f x y = draw_rect x y 50 50
let size = (size_x (), size_y ())

let rec grid x y =
  let _ = f x y in
  if x > fst size then grid 0 (y + 50)
  else if y > snd size then ()
  else grid (x + 50) y

let () = grid 0 0
let () = lineto 300 200
let () = moveto 0 600
let () = lineto 400 500
let () = moveto 600 600
let () = lineto 300 350
let () = moveto 600 0
let () = lineto 430 250
