let explode s = List.init (String.length s) (String.get s)

let sort_string s =
  s |> String.to_seq |> List.of_seq |> List.sort compare |> List.to_seq
  |> String.of_seq

module DisplayPart = struct
  type t =
    | Top
    | TopLeft
    | TopRight
    | Middle
    | BottomLeft
    | BottomRight
    | Bottom

  let compare = compare

  let to_string = function
    | Top -> "Top"
    | TopLeft -> "TopLeft"
    | TopRight -> "TopRight"
    | Middle -> "Middle"
    | BottomLeft -> "BottomLeft"
    | BottomRight -> "BottomRight"
    | Bottom -> "Bottom"
end

module DisplayPartMap = Map.Make (DisplayPart)

module Display = struct
  include DisplayPartMap

  let create value =
    DisplayPartMap.add_seq
      (List.to_seq
         [
           (DisplayPart.Top, value);
           (DisplayPart.TopLeft, value);
           (DisplayPart.TopRight, value);
           (DisplayPart.Middle, value);
           (DisplayPart.BottomLeft, value);
           (DisplayPart.BottomRight, value);
           (DisplayPart.Bottom, value);
         ])
      DisplayPartMap.empty
end

module Decoder = struct
  type partial = { parts : char list Display.t; known : (string * int) list }

  let unsolved =
    { parts = Display.create [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ]; known = [] }

  let solved { known; parts = _ } = List.length known == 10

  let deduce_by_length { parts; known } case =
    let remove_chars existing_chars chars =
      List.filter (fun x -> not (List.mem x chars)) existing_chars
    in
    let refine positions chars p =
      Display.mapi
        (fun part last_chars ->
          if List.mem part positions then
            List.filter (fun x -> List.mem x chars) last_chars
          else remove_chars last_chars chars)
        p
    in

    match String.length case with
    | 2 ->
        {
          parts =
            refine DisplayPart.[ TopRight; BottomRight ] (explode case) parts;
          known = (case, 1) :: known;
        }
    | 3 ->
        {
          parts =
            refine
              DisplayPart.[ Top; TopRight; BottomRight ]
              (explode case) parts;
          known = (case, 7) :: known;
        }
    | 4 ->
        {
          parts =
            refine
              DisplayPart.[ TopLeft; TopRight; Middle; BottomRight ]
              (explode case) parts;
          known = (case, 4) :: known;
        }
    | 5 | 6 -> { parts; known }
    | 7 -> { parts; known = (case, 8) :: known }
    | n ->
        failwith (Printf.sprintf "impossible length of string (%d, %s)" n case)

  let deduce_by_parts { parts; known } case =
    let try_parts to_find =
      let search_chars =
        List.concat_map (fun part -> Display.find part parts) to_find
      in
      List.for_all (String.contains case) search_chars
    in
    if List.mem_assoc case known then { parts; known }
    else if try_parts DisplayPart.[ Bottom; TopRight ] then
      { parts; known = (case, 0) :: known }
    else if try_parts DisplayPart.[ Middle; TopRight ] then
      { parts; known = (case, 9) :: known }
    else if try_parts DisplayPart.[ Middle; Bottom ] then
      { parts; known = (case, 6) :: known }
    else if try_parts DisplayPart.[ Bottom ] then
      { parts; known = (case, 2) :: known }
    else if try_parts DisplayPart.[ Middle ] then
      { parts; known = (case, 5) :: known }
    else if try_parts DisplayPart.[ TopRight ] then
      { parts; known = (case, 3) :: known }
    else failwith (Printf.sprintf "unknown case %s" case)

  let of_digits clues =
    let found = List.fold_left deduce_by_length unsolved clues in
    let found = List.fold_left deduce_by_parts found clues in
    found

  let decode decoder s =
    let result = List.assoc s decoder.known in
    result
end

module Input = struct
  type t = { digits : string list; code : string list }

  let create digits code =
    { digits = List.map sort_string digits; code = List.map sort_string code }

  let parse input =
    let split_parts str = str |> String.trim |> String.split_on_char ' ' in
    match String.split_on_char '|' input with
    | [ digits; code ] -> create (split_parts digits) (split_parts code)
    | _ -> failwith "invalid input format"
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

let find_easy_cases (input : Input.t list) =
  let find_easy_case decoder input =
    match Decoder.decode decoder input with 1 | 4 | 7 | 8 -> 1 | _ -> 0
  in
  input
  |> List.map (fun i -> (i, Decoder.of_digits i.Input.digits))
  |> List.fold_left
       (fun acc (input, decoder) ->
         acc
         + List.fold_left
             (fun acc code -> acc + find_easy_case decoder code)
             0 input.Input.code)
       0

let find_full_results (input : Input.t list) =
  let find_case decoder input =
    List.fold_left2
      (fun acc s i -> acc + (i * Decoder.decode decoder s))
      0 input.Input.code [ 1000; 100; 10; 1 ]
  in
  input
  |> List.map (fun i -> (i, Decoder.of_digits i.Input.digits))
  |> List.map (fun (i, decoder) -> find_case decoder i)
  |> List.fold_left ( + ) 0

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
          test_case "Example (2)" `Quick (fun () ->
              let initial = demo_input in
              (check int) "" 61229 (find_full_results initial));
          test_case "Example (2)" `Quick (fun () ->
              let initial = input "inputs/day8.txt" in
              (check int) "" 61229 (find_full_results initial));
        ] );
    ]
