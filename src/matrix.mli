type 'a matrix
(** ['a matrix] represents a matrix with elements of type 'a *)

val empty : 'a matrix
(** [empty] is an empty matrix with no entries *)

val basic_matrix : int -> int -> 'a -> 'a matrix
(** [basic_matrix row col elem] creates a matrix with [row] rows and [col]
    columns. Matrix is created with same [elem] elements within each entry.
    Requires: [row] and [col] are non-negative. *)

val add_entry : int -> int -> 'a -> 'a matrix -> 'a matrix
(** [add_entry row
   col entry matrix] adds the specified entry into the matrix
    at the defined [row] and [col], starting with an index of 0 (row 0 is first
    row). Requires: [row] and [col] must be defined within [matrix] *)

val get_row : int -> 'a matrix -> 'a list
(** [get_row row matrix] returns a list-like representation of the row specified
    by [row] starting with an index of 0 (row 0 is first row). Requires: [row]
    must be within number of rows defined within [matrix] *)

val add_row : 'a list -> 'a matrix -> 'a matrix
(** [get_row row_list matrix] adds a row specified by [row_list] to the bottom
    of the matrix. Requires: [row_list] must be equivalent to the length of all
    other rows. If added to an empty matrix, can be of any length. *)

val row_length : 'a matrix -> int
(** [row_length matrix] returns the number of rows within the matrix. *)

val get_entry : int -> int -> 'a matrix -> 'a
(** [get_entry row col matrix] returns the element at the specified row and
    column within the matrix starting with an index of 0 (row 0 is first row).
    Requires: [row] and [col] must be within number of rows and columns defined
    within [matrix] *)

val to_string : ('a -> string) -> 'a matrix -> string
(** [to_string matrix printer] creates a string representation of the matrix
    using an element printer to stringify each element *)

val to_list : 'a matrix -> 'a list list
(** [to_list matrix] creates a list-like representation of a matrix through a
    list of list of elements 'a. Each entry in the list of lists corresponds to
    a row in [matrix]. *)
