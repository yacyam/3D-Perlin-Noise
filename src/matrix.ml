type 'a matrix = 'a list list
(* AF: The list of lists [[a1; ...; an]; ... [am; ...; ap]] represents the
     matrix
     | a1 ... an |
     ...
     | am ... ap |
   where each row of the matrix corresponds to each list within the list of
   lists. The empty matrix | | corresponds to the empty list of lists [ ]*)
[@@ocamlformat "wrap-comments=false"]

let empty = []

(** [create_list col elem] creates a list of elements with a length specified by
    [col] *)
let rec create_list col elem =
  if col = 0 then [] else elem :: create_list (col - 1) elem

let rec basic_matrix_aux row col elem aux =
  if row = 0 then aux
  else basic_matrix_aux (row - 1) col elem (create_list col elem :: aux)

let basic_matrix row col elem = basic_matrix_aux row col elem empty
let row_length matrix = List.length matrix

(** [add_row row_list matrix] adds the row_list to the very end of the matrix's
    list. *)
let add_row (row_list : 'a list) matrix = matrix @ [ row_list ]

(** [add_to_row col mat_row elem] replaces the element at the specified [col]
    with [elem]. Recursively iterates through list and decrements [col] until
    the desired column is reached, in which the element at that column is
    replaced and the rest of the list is recursively appended with [elem] in
    place. *)
let rec add_to_row col mat_row elem =
  match mat_row with
  | h :: t -> if col = 0 then elem :: t else h :: add_to_row (col - 1) t elem
  | _ -> failwith "add_entry was called with incorrect column size"

(** [add_entry row col elem matrix] adds the entry [elem] at the specified row
    and column. Recursively runs through matrix until desired row is obtained
    (once row = 0), and then add_entry is called to add entry to specified row,
    where that new row is appened back into the rest of the matrix. *)
let rec add_entry row col elem matrix =
  match matrix with
  | h :: t ->
      if row = 0 then add_to_row col h elem :: t
      else h :: add_entry (row - 1) col elem t
  | _ -> failwith "add_entry was called with incorrect row size"

let get_row row matrix =
  match List.nth_opt matrix row with
  | Some row' -> row'
  | None -> failwith "get_row was called with incorrect row size"

let get_entry row col matrix =
  match List.nth_opt matrix row with
  | Some row' -> (
      match List.nth_opt row' col with
      | Some entry -> entry
      | None -> failwith "get_entry was called with incorrect col size")
  | None -> failwith "get_entry was called with incorrect row size"

(** [to_string_elems mat_row printer] creates a string for the specific matrix
    row, where [printer] stringifies the element inside matrix *)
let to_string_elems mat_row printer =
  List.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ printer elem else acc ^ ";" ^ printer elem)
    "[" mat_row
  ^ "]"

let to_string printer matrix =
  List.fold_left
    (fun acc elem ->
      if acc = "[" then acc ^ to_string_elems elem printer
      else acc ^ "," ^ "\n" ^ to_string_elems elem printer)
    "[" matrix
  ^ "]"

let to_list matrix = matrix
