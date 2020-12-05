open Core

(** Represents our boarding passes. *)
type boarding_pass = { row: int;
                       col: int;
                       id: int; }

(** Finds a row/col given a string of moves, an inital low-high pair for the seat 
    numbers, and character to match for the lower half. 
    The result is the numbers that matches the seats row or col. *)
let find_rc str ~init clow = String.fold str ~init ~f:(fun (a, b) c ->
  let diff = (b - a + 1) / 2 in
  if Char.(c = clow) then (a, b - diff)
  else (a + diff, b))
  |> fst

(** Parses boarding passes. *)
let parse_boarding_pass line =
  let row_string = String.slice line 0 7 in
  let col_string = String.slice line 7 0 in
  let row = find_rc row_string ~init:(0, 127) 'F' in
  let col = find_rc col_string ~init:(0, 7) 'L' in
  { row; col; id = row * 8 + col }

(** Finds the missing seat on the plane by looking for a difference to two
    between seat ids. 
    Returns the missing seat id. *)
let find_seat passes =
  let spasses = List.sort passes ~compare:(fun a b -> compare a.id b.id) in
  List.fold_until spasses 
    ~init:((List.hd_exn spasses).id)
    ~f:(fun prev cur ->
      if cur.id - prev = 2 
      then Stop (prev + 1)
      else Continue cur.id)
    ~finish:(fun x -> x)

let solution filename =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  let passes = List.map lines ~f:parse_boarding_pass in
  let p1 = Option.value_exn (List.max_elt passes ~compare:(fun a b -> compare a.id b.id)) in
  let p2 = find_seat passes in
  printf "Part 1: %d\nPart 2: %d\n%!" p1.id p2
