let parse line = line |> String.to_seq |> List.of_seq

let commands filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let line_pairs line =
  List.map
    (fun a -> match a with '0' -> (1, 0) | '1' -> (0, 1) | _ -> (0, 0))
    line

let sum_pairs_for_lines (mapper : int * int -> char) (pairs : char list list) :
    char list =
  let line =
    pairs |> List.map line_pairs
    |> List.fold_left
         (fun acc line ->
           if acc == [] then line
           else
             List.combine acc line
             |> List.map (fun ((sum_0, sum_1), (line_0, line_1)) ->
                    (sum_0 + line_0, sum_1 + line_1)))
         []
  in
  List.map mapper line

let common_element ((zeros, ones) : int * int) =
  if zeros > ones then '0' else '1'

let least_common_element ((zeros, ones) : int * int) =
  if zeros > ones then '1' else '0'

let binary_of_char_list chars =
  let s = chars |> List.to_seq |> String.of_seq in
  Scanf.sscanf ("0b0" ^ s) "%i" Fun.id

let rec aux common_element_eval pairs heads =
  let most_common_value =
    List.hd (sum_pairs_for_lines common_element_eval pairs)
  in
  let filtered_pairs =
    List.filter (fun item -> List.hd item == most_common_value) pairs
  in
  match filtered_pairs with
  | [] -> raise (Failure "Empty list")
  | [ item ] -> List.append (List.rev heads) item |> binary_of_char_list
  | items ->
      (aux [@tailcall]) common_element_eval (List.map List.tl items)
        (most_common_value :: heads)

let get_rating common_element_eval lines =
  aux common_element_eval (lines |> List.map parse) []

let oxygen_generator_rating input = get_rating common_element input

let co2_scrubber_rating input = get_rating least_common_element input

let life_support_rating input =
  oxygen_generator_rating input * co2_scrubber_rating input

let gamma_rate input =
  input |> List.map parse
  |> sum_pairs_for_lines common_element
  |> binary_of_char_list

let epsilon_rate input =
  input |> List.map parse
  |> sum_pairs_for_lines least_common_element
  |> binary_of_char_list

let power_consumption input = gamma_rate input * epsilon_rate input

let () =
  let open Alcotest in
  run "Day2"
    [
      ( "Parsing lines",
        [
          test_case "Simple string" `Quick (fun () ->
              Alcotest.(check (list char))
                ""
                [ '1'; '0'; '1'; '0'; '1'; '1' ]
                (parse "101011"));
        ] );
      ( "Summing elements in a line",
        [
          test_case "No elements" `Quick (fun () ->
              Alcotest.(check (list (pair int int))) "" [] (line_pairs []));
          test_case "One zero" `Quick (fun () ->
              Alcotest.(check (list (pair int int)))
                "" [ (1, 0) ]
                (line_pairs [ '0' ]));
          test_case "One one" `Quick (fun () ->
              Alcotest.(check (list (pair int int)))
                "" [ (0, 1) ]
                (line_pairs [ '1' ]));
          test_case "Multiple chars" `Quick (fun () ->
              Alcotest.(check (list (pair int int)))
                ""
                [ (1, 0); (0, 1); (0, 1); (1, 0) ]
                (line_pairs [ '0'; '1'; '1'; '0' ]));
        ] );
      ( "Summing all line pairs",
        [
          test_case "No elements" `Quick (fun () ->
              Alcotest.(check (list char))
                "" []
                (sum_pairs_for_lines common_element []));
          test_case "One line" `Quick (fun () ->
              Alcotest.(check (list char))
                "" [ '1'; '0' ]
                (sum_pairs_for_lines common_element [ [ '1'; '0' ] ]));
          test_case "Multiple lines" `Quick (fun () ->
              Alcotest.(check (list char))
                "" [ '1'; '0' ]
                (sum_pairs_for_lines common_element
                   [ [ '1'; '0' ]; [ '0'; '0' ]; [ '1'; '0' ]; [ '1'; '1' ] ]));
          test_case "Most common bits" `Quick (fun () ->
              Alcotest.(check (list char))
                "" [ '1'; '1'; '0'; '1' ]
                (sum_pairs_for_lines common_element
                   (List.map parse [ "0111"; "1000"; "1101" ])));
        ] );
      ( "Convert list to binary number",
        [
          test_case "No elements" `Quick (fun () ->
              Alcotest.(check int) "" 0b0 (binary_of_char_list []));
          test_case "Single character" `Quick (fun () ->
              Alcotest.(check int) "" 0b1 (binary_of_char_list [ '1' ]));
          test_case "Multiple characters" `Quick (fun () ->
              Alcotest.(check int)
                "" 0b10101
                (binary_of_char_list [ '1'; '0'; '1'; '0'; '1' ]));
        ] );
      ( "Getting ratings",
        [
          test_case "Single element" `Quick (fun () ->
              Alcotest.(check int)
                "" 0b0111
                (get_rating common_element [ "0111" ]));
          test_case "Two elements -> returns with '1'" `Quick (fun () ->
              Alcotest.(check int)
                "" 0b1000
                (get_rating common_element [ "0111"; "1000" ]));
          test_case "Three elements -> eliminates least common first bit" `Quick
            (fun () ->
              Alcotest.(check int)
                "" 0b01101
                (get_rating common_element [ "0111"; "1000"; "1101" ]));
          test_case
            "Least common element -> returns least common biasing towards 0"
            `Quick (fun () ->
              Alcotest.(check int)
                "" 0b00111
                (get_rating least_common_element [ "0111"; "1111" ]));
          test_case "Three elements -> eliminates most common first bit" `Quick
            (fun () ->
              Alcotest.(check int)
                "" 0b0111
                (get_rating least_common_element [ "0111"; "1000"; "1101" ]));
          test_case "Based on example (most common)" `Quick (fun () ->
              Alcotest.(check int)
                "" 0b10111
                (get_rating common_element
                   [
                     "00100";
                     "11110";
                     "10110";
                     "10111";
                     "10101";
                     "01111";
                     "00111";
                     "11100";
                     "10000";
                     "11001";
                     "00010";
                     "01010";
                   ]));
        ] );
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              Alcotest.(check int)
                "" 198
                (power_consumption
                   [
                     "00100";
                     "11110";
                     "10110";
                     "10111";
                     "10101";
                     "01111";
                     "00111";
                     "11100";
                     "10000";
                     "11001";
                     "00010";
                     "01010";
                   ]));
          test_case "Real (1)" `Quick (fun () ->
              Alcotest.(check int)
                "" 4118544
                (commands "inputs/day3.txt" |> power_consumption));
          test_case "Example (2)" `Quick (fun () ->
              Alcotest.(check int)
                "" 230
                (life_support_rating
                   [
                     "00100";
                     "11110";
                     "10110";
                     "10111";
                     "10101";
                     "01111";
                     "00111";
                     "11100";
                     "10000";
                     "11001";
                     "00010";
                     "01010";
                   ]));
          test_case "Real (2)" `Quick (fun () ->
              Alcotest.(check int)
                "" 3832770
                (commands "inputs/day3.txt" |> life_support_rating));
        ] );
    ]
