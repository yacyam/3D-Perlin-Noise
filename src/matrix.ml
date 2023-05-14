type 'a t = 'a array array
(* AF: The array of arrays [|[|a1; ...; an|]; ... [|am; ...; ap|]|] represents
    the matrix
     | a1 ... an |
     ...
     | am ... ap |
   where each row of the matrix corresponds to each array within the array of
   arrays. The empty matrix | | corresponds to the empty array of arrays [||]*)
[@@ocamlformat "wrap-comments=false"]

let empty = [||]
let basic_matrix row col elem = Array.make_matrix row col elem
let row_length matrix = Array.length matrix

(** [add_row row_list matrix] adds the row_list to the very bottom of the
    matrix. If the matrix is empty, then [row_list] becomes first row. *)
let add_row (row_list : 'a list) matrix =
  Array.append matrix [| Array.of_list row_list |]

(** [add_entry row col elem matrix] adds the entry [elem] at the specified row
    and column. Utilizes mutability to specify exact row and column in O(1) *)
let rec add_entry row col elem matrix =
  matrix.(row).(col) <- elem;
  matrix

let get_row row matrix =
  match Array.get matrix row with
  | row' -> Array.to_list row'
  | exception _ -> failwith "get_row was called with incorrect row size"

let get_entry row col matrix =
  match Array.get matrix row with
  | row' -> (
      match Array.get row' col with
      | entry -> entry
      | exception _ -> failwith "get_entry was called with incorrect col size")
  | exception _ -> failwith "get_entry was called with incorrect row size"

let rec map_helper f matrix row =
  match Array.get matrix row with
  | row' ->
      matrix.(row) <- Array.map f row';
      map_helper f matrix (row + 1)
  | exception _ -> ()

let map f matrix =
  map_helper f matrix 0;
  matrix

(** [to_string_elems mat_row printer] creates a string for the specific matrix
    row, where [printer] stringifies the element inside matrix *)
let to_string_elems mat_row printer =
  Array.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ printer elem else acc ^ ";" ^ printer elem)
    "[" mat_row
  ^ "]"

let to_string printer matrix =
  Array.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ to_string_elems elem printer
      else acc ^ "," ^ "\n" ^ to_string_elems elem printer)
    "[" matrix
  ^ "]"

let to_list matrix =
  Array.fold_left (fun acc row -> Array.to_list row :: acc) [] matrix
  |> List.rev
