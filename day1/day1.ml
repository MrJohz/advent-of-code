let heads window list =
  let rec inner window list heads =
    if window <= 0 then (heads, list)
    else
      match list with
      | [] -> (heads, list)
      | head :: tail -> inner (window - 1) tail (head :: heads)
  in
  inner window list []

let sliding_window_sum window list =
  let rec inner list acc =
    if List.length list < window then acc
    else
      let head, _ = heads window list in
      inner (List.tl list) (List.fold_left ( + ) 0 head :: acc)
  in
  List.rev (inner list [])

let test_sliding_window_one () =
  Alcotest.(check (list int))
    "" [ 1; 2; 3; 4 ]
    (sliding_window_sum 1 [ 1; 2; 3; 4 ])

let test_sliding_window_two () =
  Alcotest.(check (list int))
    "" [ 3; 5; 7 ]
    (sliding_window_sum 2 [ 1; 2; 3; 4 ])

let test_sliding_window_too_many () =
  Alcotest.(check (list int)) "" [] (sliding_window_sum 8 [ 1; 2; 3; 4 ])

let count_increments list =
  let rec inner list acc =
    match list with
    | [] -> acc
    | [ _ ] -> acc
    | a :: b :: tail -> inner (b :: tail) (if b > a then acc + 1 else acc)
  in
  inner list 0

let test_finds_increments () =
  Alcotest.(check int) "" 1 (count_increments [ 1; 2 ])

let test_ignores_decrements () =
  Alcotest.(check int) "" 0 (count_increments [ 2; 1 ])

let test_run_of_elements () =
  Alcotest.(check int) "" 3 (count_increments [ 2; 1; 4; 2; 4; 3; 6 ])

let test_from_exercise () =
  Alcotest.(check int)
    "" 7
    (count_increments [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ])

let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let true_test () =
  let channel = open_in "inputs/day1.txt" in
  let stream =
    Stream.from (fun _ ->
        try Some (int_of_string (input_line channel)) with End_of_file -> None)
  in
  Alcotest.(check int) "" 1167 (count_increments (list_of_stream stream))

let test_from_exercise_with_window () =
  Alcotest.(check int)
    "" 5
    (count_increments
       (sliding_window_sum 3
          [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ]))

let true_test_2 () =
  let channel = open_in "inputs/day1.txt" in
  let stream =
    Stream.from (fun _ ->
        try Some (int_of_string (input_line channel)) with End_of_file -> None)
  in
  Alcotest.(check int)
    "" 1130
    (count_increments (sliding_window_sum 3 (list_of_stream stream)))

let () =
  let open Alcotest in
  run "Day1"
    [
      ( "Basic",
        [
          test_case "Simple increment" `Quick test_finds_increments;
          test_case "Simple decrement" `Quick test_ignores_decrements;
          test_case "Small run" `Quick test_run_of_elements;
        ] );
      ( "Sliding Window",
        [
          test_case "window size 1" `Quick test_sliding_window_one;
          test_case "window size 2" `Quick test_sliding_window_two;
          test_case "window size larger than array" `Quick
            test_sliding_window_too_many;
        ] );
      ( "Exercise",
        [
          test_case "From exercise" `Quick test_from_exercise;
          test_case "True test" `Quick true_test;
          test_case "From exercise (2)" `Quick test_from_exercise_with_window;
          test_case "True test (2)" `Quick true_test_2;
        ] );
    ]
