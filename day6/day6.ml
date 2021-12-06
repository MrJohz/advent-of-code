module Lanternfish = struct
  type t = int

  let parse = int_of_string

  let grow age = match age with 0 -> [ 6; 8 ] | n -> [ n - 1 ]
end

module Shoal = struct
  type t = {
    zero : int;
    one : int;
    two : int;
    three : int;
    four : int;
    five : int;
    six : int;
    seven : int;
    eight : int;
  }

  let empty =
    {
      zero = 0;
      one = 0;
      two = 0;
      three = 0;
      four = 0;
      five = 0;
      six = 0;
      seven = 0;
      eight = 0;
    }

  let count shoal =
    shoal.zero + shoal.one + shoal.two + shoal.three + shoal.four + shoal.five
    + shoal.six + shoal.seven + shoal.eight

  let parse l =
    let add_to_shoal n shoal =
      match n with
      | 0 -> { shoal with zero = shoal.zero + 1 }
      | 1 -> { shoal with one = shoal.one + 1 }
      | 2 -> { shoal with two = shoal.two + 1 }
      | 3 -> { shoal with three = shoal.three + 1 }
      | 4 -> { shoal with four = shoal.four + 1 }
      | 5 -> { shoal with five = shoal.five + 1 }
      | 6 -> { shoal with six = shoal.six + 1 }
      | 7 -> { shoal with seven = shoal.seven + 1 }
      | 8 -> { shoal with eight = shoal.eight + 1 }
      | _ -> assert false
    in
    let rec aux shoal fishes =
      match fishes with
      | [] -> shoal
      | head :: tail -> aux (add_to_shoal head shoal) tail
    in
    aux empty l

  let grow { zero; one; two; three; four; five; six; seven; eight } =
    {
      zero = one;
      one = two;
      two = three;
      three = four;
      four = five;
      five = six;
      six = seven + zero;
      seven = eight;
      eight = zero;
    }
end

let rec generations n shoal =
  match n with 0 -> shoal | n -> generations (n - 1) (Shoal.grow shoal)

let input filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  stream |> Stream.next |> String.split_on_char ',' |> List.map int_of_string

let () =
  let open Alcotest in
  run "Day5"
    [
      ( "Problem Cases",
        [
          test_case "Example (0.1)" `Quick (fun () ->
              let initial = [ 3; 4; 3; 1; 2 ] in
              Alcotest.(check int)
                "" 26
                (Shoal.count (generations 18 (Shoal.parse initial))));
          test_case "Example (1)" `Quick (fun () ->
              let initial = [ 3; 4; 3; 1; 2 ] in
              Alcotest.(check int)
                "" 5934
                (Shoal.count (generations 80 (Shoal.parse initial))));
          test_case "Real (1)" `Quick (fun () ->
              let initial = input "inputs/day6.txt" in
              Alcotest.(check int)
                "" 356190
                (Shoal.count (generations 80 (Shoal.parse initial))));
          test_case "Example (2)" `Quick (fun () ->
              let initial = [ 3; 4; 3; 1; 2 ] in
              Alcotest.(check int)
                "" 26984457539
                (Shoal.count (generations 256 (Shoal.parse initial))));
          test_case "Real (1)" `Quick (fun () ->
              let initial = input "inputs/day6.txt" in
              Alcotest.(check int)
                "" 1617359101538
                (Shoal.count (generations 256 (Shoal.parse initial))));
        ] );
    ]
