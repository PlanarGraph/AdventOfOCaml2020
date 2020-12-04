open Core

(* The following implementations brute-force the problem, which works because
   this is the first and possibly easiest day. There are some faster methods
   (binary search w/ array for part 1) that come to mind, but I'll keep my
   original and immediate implementations up for posterity. *)

(** A messy brute-force recursive implementation that searches each element [x] of 
    our input list, checking if there exists a value [y] in the remainder of the 
    list such that [x + y = v]. 

    Returns [Some z] where [z = x * y] for the first two values [x] and [y] that 
    satisfy [x + y = v].
    Otherwise returns None.*)
let part1 v lst = 
  let rec poss x = function
    | [] -> None
    | y :: _ when x + y = v -> Some y
    | _ :: ys -> poss x ys
  in
  let rec find = function
    | [] | [_] -> None
    | x :: xs ->
      match poss x xs with
      | Some s -> Some (s * x)
      | None -> find xs
  in

  find lst

(** A simpler monadic implementation of the problem. Finds three distinct values
    [x], [y], and [z] from the input list [lst] such that [x + y + z]. It's
    fairly similar to a list comprehension in haskell.
    
    Returns a list containing all of the possible solutions [x * y * z]. *)
let part2 v lst =
  let open List.Let_syntax in
  let%bind x = lst in
  let%bind y = lst in
  let%bind z = lst in
  if x <> y && x <> z && y <> z && x + y + z = v then 
    ret\rn (x * y * z)
  else
    []

let solution filename =
  let file = In_channel.create filename in
  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
  let p1 = part1 2020 numbers |> Option.value_exn in
  let p2 = part2 2020 numbers |> List.hd_exn in
  printf "Part 1: %d\nPart2: %d\n%!" p1 p2
