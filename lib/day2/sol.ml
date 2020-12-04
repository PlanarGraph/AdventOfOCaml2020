open Core


(** Represents a corporate policy for passwords. *)
type policy = { low: int;  (** The lowest number of the policy. *)
                high: int; (** The highest number of the policy. *)
                c: char;   (** The character associated with the policy. *)
              }

(** A simple parser for reading a policy from a string. 
    Returns [Some policy] if the string contains a valid policy.
    Otherwise, returns [None]. *)
let parse_policy s =
  let open Option.Let_syntax in
  let%bind (nums, ch) = String.lsplit2 s ~on:' ' in
  let%bind (min, max) = String.lsplit2 nums ~on:'-' in
  Some { low = Int.of_string min;
         high = Int.of_string max;
         c = ch.[0];
       }

(** Parses a line of input data into a policy and a given password. 
    Returns [Some (policy, password)] if the string is properly formatted input.
    Otherwise, returns [None]. *)
let parse_data line =
  let open Option.Let_syntax in
  let%bind (pol, wpass) = String.lsplit2 line ~on:':' in
  let%bind policy = parse_policy pol in
  let pass = String.strip wpass in
  Some (policy,  pass)

(** A validator for part one of today's puzzle.
    Given a [policy] and a [password], this counts the number of occurrences of
    [policy.c] in [password]: [nc] and verifies that 
    [policy.low <= nc <= policy.high].
    Returns [true] if the condition holds, [false] otherwise. *)
let validate_part1 (policy, password) =
  String.filter ~f:(fun c -> Char.(c = policy.c)) password
  |> String.length
  |> (fun v -> v >= policy.low && v <= policy.high)


(** A vaidator for part two of today's puzzle.
    Given a [policy] and a [password], this verifies that exactly one of
    [password.[policy.low]] or [password.[policy.high]] is equal to [policy.c].
    Returns [true] if the condition holds, [false] otherwise. *)
let validate_part2 (policy, password) =
  let open Char in
  (password.[policy.low - 1] = policy.c && password.[policy.high - 1] <> policy.c)
  || (password.[policy.low - 1] <> policy.c && password.[policy.high - 1] = policy.c)

(** A helper function that filters valid [(policy, password)] pairs,
    applies them to the [validator], and returns the number of valid entries. *)
let valid_data data validator =
  List.filter_opt data
  |> List.filter ~f:validator
  |> List.length

let solution filename =
  let file = In_channel.create filename in
  let data = List.map ~f:parse_data (In_channel.input_lines file) in
  let part1 = valid_data data validate_part1 in
  let part2 = valid_data data validate_part2 in
  printf "Part 1: %d\nPart 2: %d\n%!" part1 part2
