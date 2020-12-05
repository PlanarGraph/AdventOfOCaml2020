open Core

(** Represents different units of measure. *)
type units =
  | In
  | Cm

(** Represents different eye colours. *)
type ec =
  | Amb
  | Blu
  | Brn
  | Gry
  | Grn
  | Hzl
  | Oth

module Part1 = struct
  (** Represents the passport in part 1. *)
  type t = { mutable byr: string option;
             mutable iyr: string option;
             mutable eyr: string option;
             mutable hgt: string option;
             mutable hcl: string option;
             mutable ecl: string option;
             mutable pid: string option;
             mutable cid: string option; }

  (** Validates that all required values are present. *)
  let validate passport =
    Option.(is_some passport.byr &&
            is_some passport.iyr &&
            is_some passport.eyr &&
            is_some passport.hgt &&
            is_some passport.hcl &&
            is_some passport.ecl &&
            is_some passport.pid)
end

module Part2 = struct
  (** Represents the valid passport in part 2. *)
  type t = { byr: int;
             iyr: int;
             eyr: int;
             hgt: (units * int);
             hcl: string;
             ecl: ec;
             pid: int; 
             cid: int option; }
end

(** Splits a string in two parts on a certain character.
    Raises an exception if more than two splits can be made with
    the current string/char given. *)
let split_pair str ~on =
  match String.split str ~on with
  | [a; b] -> (a, b)
  | _ -> failwith "not a pair"

(** Parses height values specified as "[x]in" or "[x]cm", where [x] is some integer 
    value. 
    Returns an option containing the pair [(measure, x)], where measure is one of [Cm] 
    or [In]. *)
let parse_height value =
  let h = String.strip value ~drop:Char.is_alpha in
  let utt = String.strip value ~drop:Char.is_digit in
  let height = Int.of_string h in
  match utt with
  | "cm" when height >= 150 && height <= 193 -> Some (Cm, height)
  | "in" when height >= 59 && height <= 76 -> Some (In, height)
  | _ -> None

(** This function currently just validates that given string is a valid hex string.
    Returns an option containing the string with the ['#'] stripped if it is valid. *)
let parse_hex value = 
  if Char.(value.[0] <> '#') then None
  else
    let vl = String.strip value ~drop:(fun c -> Char.(c = '#')) in
    if String.length vl = 6 && 
      String.for_all vl ~f:(fun c -> Char.(is_digit c || (c >= 'a' && c <= 'f'))) then
      Some vl
    else
      None

(** Parses the eye colour. *)
let parse_ecl value = match value with
  | "amb" -> Some Amb
  | "blu" -> Some Blu
  | "brn" -> Some Brn
  | "gry" -> Some Gry
  | "grn" -> Some Grn
  | "hzl" -> Some Hzl
  | "oth" -> Some Oth
  | _ -> None
  
(** Parses a passport that contains all required fields for part 1.
    Returns a Part1.t option. *)
let parse_passport_p1 lines =
  let open Part1 in
  let passport = { byr = None;
                   iyr = None;
                   eyr = None;
                   hgt = None;
                   hcl = None;
                   ecl = None;
                   pid = None;
                   cid = None;
                 }
  in
  List.iter lines ~f:(fun line ->
      let pairs = String.split line ~on:' ' in
      List.iter pairs ~f:(fun pair ->
        let (field, value) = split_pair pair ~on:':' in
        match field with
        | "byr" -> passport.byr <- Some value 
        | "iyr" -> passport.iyr <- Some value 
        | "eyr" -> passport.eyr <- Some value
        | "hgt" -> passport.hgt <- Some value
        | "hcl" -> passport.hcl <- Some value
        | "ecl" -> passport.ecl <- Some value
        | "pid" -> passport.pid <- Some value
        | _ -> ()
        ));
  if validate passport then Some passport
  else None

(** Helper function to parse an int given a condition function [~f]. *)
let parse_yr vl ~f =
  try
    let v = Int.of_string vl in
    if f v then Some v else None
  with _ -> None

(** Monadic bind helper that wraps [parse_yr]. Eliminates some duplicate code
    from [parse_passport_p2]. *)
let bps_yr vl ~f = Option.bind vl ~f:(parse_yr ~f)

(** Parses a valid passport for part 2 in a monadic style. 
    Returns an option containing a valid passport, or None if the passport is invalid. *)
let parse_passport_p2 (passport: Part1.t) =
  let open Part2 in
  let open Option.Let_syntax in
  let%bind byr = bps_yr passport.byr ~f:(fun v -> v >= 1920 && v <= 2002) in
  let%bind iyr = bps_yr passport.iyr ~f:(fun v -> v >= 2010 && v <= 2020) in
  let%bind eyr = bps_yr passport.eyr ~f:(fun v -> v >= 2020 && v <= 2030) in
  let%bind hgt = Option.bind passport.hgt ~f:parse_height in 
  let%bind hcl = Option.bind passport.hcl ~f:parse_hex in
  let%bind ecl = Option.bind passport.ecl ~f:parse_ecl in
  let%bind pid = Option.bind passport.pid ~f:(fun value ->
    if String.length value = 9 then try Some(Int.of_string value) with _ -> None
    else None)
  in
  Some { byr; iyr; eyr; hgt; hcl; ecl; pid; cid = None }

(** Helper function to parse empty line separated passports from a list of strings.
    Returns all valid passports that conform to the spec given for part1. *)
let rec parse_passports pf = function
  | [] -> []
  | lines ->
    let (p, ps) = List.split_while lines ~f:(fun line -> String.(line <> "")) in
    let pass = pf p in
    let rest = parse_passports pf (List.tl ps |> Option.value ~default:[]) in
    match pass with
    | None -> rest
    | Some p -> p :: rest

let solution filename =
  let file = In_channel.create filename in
  let lines = In_channel.input_lines file in
  let p1 = parse_passports parse_passport_p1 lines in
  let p2 = List.filter_map p1 ~f:parse_passport_p2 in
  printf "Part 1: %d\nPart 2: %d\n%!" (List.length p1) (List.length p2)
