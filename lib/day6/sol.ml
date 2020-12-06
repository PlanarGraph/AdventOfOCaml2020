open Core

(** Parses the different blank newline separated question blocks.
    It's reused code from day 4 for the most part. 
    Returns a list of list of strings. *)
let rec parse_questions = function
  | [] -> []
  | lines ->
    let (p, ps) = List.split_while lines ~f:(fun line -> String.(line <> "")) in
    let rest = parse_questions (List.tl ps |> Option.value ~default:[]) in
    p :: rest

(** Counts the number questions answered in the slist based on some reduction function 
    for sets. For part 1 set union is used as we only care about what questions have 
    been answered by some person, and in part 2 we use set intersection. *)
let count slist rf =
  let empty = Set.empty (module Char) in
  List.map slist ~f:(fun s -> String.fold s ~init:empty ~f:Set.add)
  |> List.reduce ~f:rf
  |> Option.value ~default:empty
  |> Set.length

let solution filename =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  let all_questions = parse_questions lines in
  let p1 = List.fold all_questions ~init:0 ~f:(fun b a -> b + count a Set.union) in
  let p2 = List.fold all_questions ~init:0 ~f:(fun b a ->b + count a Set.inter) in
  printf "Part 1: %d\nPart 2: %d\n%!" p1 p2
