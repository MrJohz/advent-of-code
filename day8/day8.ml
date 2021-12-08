module Display = struct
  type t = {
    top : bool;
    top_left : bool;
    top_right : bool;
    middle : bool;
    bottom_left : bool;
    bottom_right : bool;
    bottom : bool;
  }

  let blank =
    {
      top = false;
      top_left = false;
      top_right = false;
      middle = false;
      bottom_left = false;
      bottom_right = false;
      bottom = false;
    }

  let full =
    {
      top = true;
      top_left = true;
      top_right = true;
      middle = true;
      bottom_left = true;
      bottom_right = true;
      bottom = true;
    }

  let of_int i =
    match i with
    | 0 -> { full with middle = false }
    | 1 -> { blank with top_right = true; bottom_right = true }
    | 2 -> { full with top_left = false; bottom_right = false }
    | 3 -> { full with top_left = false; bottom_left = false }
    | 4 -> { full with top = false; bottom_left = false; bottom = false }
    | 5 -> { full with top_right = false; bottom_left = false }
    | 6 -> { full with top_right = false }
    | 7 -> { blank with top = true; top_right = true; bottom_right = true }
    | 8 -> full
    | 9 -> { full with bottom_left = false }
    | _ -> failwith "i must be a single digit"

  let count_segments display =
    Bool.to_int display.top
    + Bool.to_int display.top_left
    + Bool.to_int display.top_right
    + Bool.to_int display.middle
    + Bool.to_int display.bottom_left
    + Bool.to_int display.bottom_right
    + Bool.to_int display.bottom
end

module Solver = struct
  type guess = Solved of int | Narrowed of int list | Unsolved

  let solve_by_length case =
    match String.length case with
    | 2 -> Solved 1
    | 3 -> Solved 7
    | 4 -> Solved 4
    | 5 -> Narrowed [ 2; 3; 5 ]
    | 6 -> Narrowed [ 6; 9; 0 ]
    | 7 -> Solved 8
    | n ->
        failwith (Printf.sprintf "impossible length of string (%d, %s)" n case)
end

module Input = struct
  type t = { digits : string list; code : string list }

  let create digits code = { digits; code }

  let parse input =
    let split_parts str = str |> String.trim |> String.split_on_char ' ' in
    match String.split_on_char '|' input with
    | [ digits; code ] ->
        { digits = split_parts digits; code = split_parts code }
    | _ -> failwith "invalid input format"

  let find_easy_cases input = List.map Solver.solve_by_length input.code
end

let input filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  result := List.rev !result;
  List.map Input.parse !result

let find_easy_cases input =
  input
  |> List.map Input.find_easy_cases
  |> List.flatten
  |> List.fold_left
       (fun acc guess -> acc + match guess with Solver.Solved _ -> 1 | _ -> 0)
       0

let demo_input =
  [
    Input.create
      [
        "be";
        "cfbegad";
        "cbdgef";
        "fgaecd";
        "cgeb";
        "fdcge";
        "agebfd";
        "fecdb";
        "fabcd";
        "edb";
      ]
      [ "fdgacbe"; "cefdb"; "cefbgd"; "gcbe" ];
    Input.create
      [
        "edbfga";
        "begcd";
        "cbg";
        "gc";
        "gcadebf";
        "fbgde";
        "acbgfd";
        "abcde";
        "gfcbed";
        "gfec";
      ]
      [ "fcgedb"; "cgb"; "dgebacf"; "gc" ];
    Input.create
      [
        "fgaebd";
        "cg";
        "bdaec";
        "gdafb";
        "agbcfd";
        "gdcbef";
        "bgcad";
        "gfac";
        "gcb";
        "cdgabef";
      ]
      [ "cg"; "cg"; "fdcagb"; "cbg" ];
    Input.create
      [
        "fbegcd";
        "cbd";
        "adcefb";
        "dageb";
        "afcb";
        "bc";
        "aefdc";
        "ecdab";
        "fgdeca";
        "fcdbega";
      ]
      [ "efabcd"; "cedba"; "gadfec"; "cb" ];
    Input.create
      [
        "aecbfdg";
        "fbg";
        "gf";
        "bafeg";
        "dbefa";
        "fcge";
        "gcbea";
        "fcaegb";
        "dgceab";
        "fcbdga";
      ]
      [ "gecf"; "egdcabf"; "bgf"; "bfgea" ];
    Input.create
      [
        "fgeab";
        "ca";
        "afcebg";
        "bdacfeg";
        "cfaedg";
        "gcfdb";
        "baec";
        "bfadeg";
        "bafgc";
        "acf";
      ]
      [ "gebdcfa"; "ecba"; "ca"; "fadegcb" ];
    Input.create
      [
        "dbcfg";
        "fgd";
        "bdegcaf";
        "fgec";
        "aegbdf";
        "ecdfab";
        "fbedc";
        "dacgb";
        "gdcebf";
        "gf";
      ]
      [ "cefg"; "dcbef"; "fcge"; "gbcadfe" ];
    Input.create
      [
        "bdfegc";
        "cbegaf";
        "gecbf";
        "dfcage";
        "bdacg";
        "ed";
        "bedf";
        "ced";
        "adcbefg";
        "gebcd";
      ]
      [ "ed"; "bcgafe"; "cdgba"; "cbgef" ];
    Input.create
      [
        "egadfb";
        "cdbfeg";
        "cegd";
        "fecab";
        "cgb";
        "gbdefca";
        "cg";
        "fgcdab";
        "egfdb";
        "bfceg";
      ]
      [ "gbdfcae"; "bgc"; "cg"; "cgb" ];
    Input.create
      [
        "gcafb";
        "gcf";
        "dcaebfg";
        "ecagb";
        "gf";
        "abcdeg";
        "gaef";
        "cafbge";
        "fdbac";
        "fegbdc";
      ]
      [ "fgae"; "cfgab"; "fg"; "bagce" ];
  ]

let () =
  let open Alcotest in
  run "Day5"
    [
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              let initial = demo_input in
              (check int) "" 26 (find_easy_cases initial));
          test_case "Real (1)" `Quick (fun () ->
              let initial = input "inputs/day8.txt" in
              (check int) "" 449 (find_easy_cases initial));
          (* test_case "Example (2)" `Quick (fun () ->
                 let initial = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] in
                 (check int) "" 168 (naive Crab.move_2 initial));
             test_case "Real (2)" `Quick (fun () ->
                 let initial = input "inputs/day7.txt" in
                 (check int) "" 0 (naive Crab.move_2 initial)); *)
        ] );
    ]
