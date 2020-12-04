open Core

(** Recursively counts trees encountered given a [grid], rates of change [dx] and [dy],
    and a starting point [(x, y)].

    Returns the total number of trees encountered on the path. *)
let rec count_trees grid dx dy (x, y) =
  (* If we're below the grid, we're done sliding. *)
  if y + dy >= Array.length grid then 
    0
  else
    let nx = (x + dx) mod (String.length grid.(0)) in
    (* Recursively count the trees in the rest of our path. *)
    let total = count_trees grid dx dy (nx, y + dy) in
    match grid.(y + dy).[nx] with
    | '#' -> total + 1 (* Add one to the total if we're on  a tree. *)
    | _ -> total

let solution filename =
  let file = In_channel.create filename in
  let grid = Array.of_list (In_channel.input_lines file) in
  (* Compute the solutions for part 1 and 2 once. *)
  let sols = List.map [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
      ~f:(fun (dx, dy) -> count_trees grid dx dy (0, 0))
  in
  let p1 = List.nth_exn sols 1 in
  let p2 = List.fold_left sols ~f:( * ) ~init:1 in
  printf "Part 1: %d\nPart 2: %d\n%!" p1 p2
