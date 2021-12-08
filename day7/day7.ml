module Crab = struct
  type t = int

  let parse = int_of_string

  let move to_pos self_pos = abs (self_pos - to_pos)

  let move_2 to_pos self_pos =
    let distance = abs (self_pos - to_pos) in
    distance * (distance + 1) / 2
end

let naive move_fn crabs =
  let score n crabs =
    List.fold_left (fun acc crab -> acc + move_fn n crab) 0 crabs
  in
  let rec aux current max best_score =
    if current == max then best_score
    else
      let current_score = score current crabs in
      aux (current + 1) max
        (if current_score < best_score then current_score else best_score)
  in
  let min, max =
    List.fold_left
      (fun (min, max) crab ->
        if crab > max then (min, crab)
        else if crab < min then (crab, max)
        else (min, max))
      (100, 100) crabs
  in
  aux min max 100000000000

let median items =
  let rec groups n items n' currentgroup itemlist =
    match items with
    | [] -> List.rev (currentgroup :: itemlist)
    | head :: items ->
        if n' == 0 then
          groups n items n [ head ] (List.rev currentgroup :: itemlist)
        else groups n items (n' - 1) (head :: currentgroup) itemlist
  in
  let simple_median list =
    List.nth (List.sort compare list) (List.length list / 2)
  in

  let rec aux items i =
    let sublists = groups 5 items 5 [] [] in
    let medians = List.map simple_median sublists in
    let pivot =
      if List.length medians <= 5 then simple_median medians
      else aux medians (List.length medians / 2)
    in
    let low, high = List.partition (fun i -> i < pivot) items in
    let k = List.length low in
    if i < k then aux low i
    else if i > k then aux high (i - k - 1)
    else List.nth items i
  in
  aux items (List.length items / 2)

let movement_plan crabs =
  let ideal_location =
    List.nth (List.sort compare crabs) (List.length crabs / 2)
  in
  Printf.printf "ideal location = %d\n" ideal_location;
  List.map (Crab.move ideal_location) crabs |> List.fold_left ( + ) 0

let movement_plan_2 crabs =
  let ideal_location =
    List.fold_left (fun acc n -> acc +. float_of_int n) 0.0 crabs
    /. float_of_int (List.length crabs)
  in
  Printf.printf "ideal location = %f\n" ideal_location;
  List.map (Crab.move_2 (int_of_float (Float.round ideal_location))) crabs
  |> List.fold_left ( + ) 0

let input filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  stream |> Stream.next |> String.split_on_char ',' |> List.map Crab.parse

let () =
  let open Alcotest in
  run "Day5"
    [
      ( "Medians",
        [
          test_case "Gets median of one-element list" `Quick (fun () ->
              (check int) "" 1 (median [ 1 ]));
          test_case "Gets median of one-element list with any value in it"
            `Quick (fun () -> (check int) "" 4 (median [ 4 ]));
          test_case "Gets median of two-element list" `Quick (fun () ->
              (check int) "" 5 (median [ 4; 5 ]));
          test_case "Gets median of two-element list with wide spread" `Quick
            (fun () -> (check int) "" 6 (median [ 4; 6 ]));
          test_case "Gets median of three-element list" `Quick (fun () ->
              (check int) "" 5 (median [ 4; 5; 7 ]));
          test_case "Gets median of reverse-sorted three-element list" `Quick
            (fun () -> (check int) "" 5 (median [ 7; 5; 4 ]));
          test_case "Gets median of a list with at least one partition in it"
            `Quick (fun () ->
              (check int) "" 5 (median [ 7; 7; 7; 7; 5; 4; 4; 4; 4 ]));
          test_case "Gets median of example inputs" `Quick (fun () ->
              (check int) "" 2 (median [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]));
        ] );
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              let initial = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] in
              (check int) "" 37 (naive Crab.move initial));
          test_case "Real (1)" `Quick (fun () ->
              let initial = input "inputs/day7.txt" in
              (check int) "" 357353 (naive Crab.move initial));
          test_case "Example (2)" `Quick (fun () ->
              let initial = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] in
              (check int) "" 168 (naive Crab.move_2 initial));
          test_case "Real (2)" `Quick (fun () ->
              let initial = input "inputs/day7.txt" in
              (check int) "" 104822130 (naive Crab.move_2 initial));
        ] );
    ]
