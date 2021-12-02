module Command = struct
  type t = Forward of int | Down of int | Up of int

  let pp ppf cmd =
    match cmd with
    | Forward n -> Fmt.pf ppf "Forward %d" n
    | Down n -> Fmt.pf ppf "Down %d" n
    | Up n -> Fmt.pf ppf "Up %d" n

  let equal x y =
    match (x, y) with
    | Forward x, Forward y -> x == y
    | Down x, Down y -> x == y
    | Up x, Up y -> x == y
    | _, _ -> false

  let testable = Alcotest.testable pp equal

  let command_of_string string =
    let parts = String.split_on_char ' ' string in
    match parts with
    | [ "forward"; digit ] ->
        Option.map (fun digit -> Forward digit) (int_of_string_opt digit)
    | [ "down"; digit ] ->
        Option.map (fun digit -> Down digit) (int_of_string_opt digit)
    | [ "up"; digit ] ->
        Option.map (fun digit -> Up digit) (int_of_string_opt digit)
    | _ -> None
end

module Position = struct
  type t = { horizontal : int; depth : int }

  let pp ppf { horizontal; depth } =
    Fmt.pf ppf "{ horizontal = %d ; depth = %d }" horizontal depth

  let equal x y = x.horizontal == y.horizontal && x.depth == y.depth

  let testable = Alcotest.testable pp equal

  let move pos command =
    match command with
    | Command.Forward n -> { pos with horizontal = pos.horizontal + n }
    | Command.Down n -> { pos with depth = pos.depth + n }
    | Command.Up n -> { pos with depth = pos.depth - n }

  let distance_mul pos = pos.depth * pos.horizontal
end

module PositionWithAim = struct
  type t = { horizontal : int; depth : int; aim : int }

  let pp ppf { horizontal; depth; aim } =
    Fmt.pf ppf "{ horizontal = %d ; depth = %d ; aim = %d }" horizontal depth
      aim

  let equal x y =
    x.horizontal == y.horizontal && x.depth == y.depth && x.aim == y.aim

  let testable = Alcotest.testable pp equal

  let move pos command =
    match command with
    | Command.Forward n ->
        {
          pos with
          horizontal = pos.horizontal + n;
          depth = pos.depth + (n * pos.aim);
        }
    | Command.Down n -> { pos with aim = pos.aim + n }
    | Command.Up n -> { pos with aim = pos.aim - n }

  let distance_mul pos = pos.depth * pos.horizontal
end

let commands filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Command.command_of_string (input_line channel)
        with End_of_file -> None)
  in
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let sum_positions move_fn initial commands =
  let rec aux commands pos =
    match commands with
    | [] -> pos
    | head :: tail -> aux tail (move_fn pos head)
  in
  aux commands initial

let () =
  let open Alcotest in
  run "Day2"
    [
      ( "Move Functionality",
        [
          test_case "Simple increment" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 4; depth = 0 }
                (Position.move { horizontal = 0; depth = 0 } (Forward 4)));
          test_case "Move from previous position" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 8; depth = 0 }
                (Position.move { horizontal = 4; depth = 0 } (Forward 4)));
          test_case "Vertical movement down has correct sign" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 0; depth = 5 }
                (Position.move { horizontal = 0; depth = 0 } (Down 5)));
          test_case "Vertical movement up has correct sign" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 0; depth = -5 }
                (Position.move { horizontal = 0; depth = 0 } (Up 5)));
        ] );
      ( "Moving With Aims",
        [
          test_case "Simple horizontal move" `Quick (fun () ->
              Alcotest.(check PositionWithAim.testable)
                ""
                { horizontal = 4; depth = 0; aim = 0 }
                (PositionWithAim.move
                   { horizontal = 0; depth = 0; aim = 0 }
                   (Forward 4)));
          test_case "Simple change in aim" `Quick (fun () ->
              Alcotest.(check PositionWithAim.testable)
                ""
                { horizontal = 0; depth = 0; aim = 5 }
                (PositionWithAim.move
                   { horizontal = 0; depth = 0; aim = 0 }
                   (Down 5)));
          test_case "Moving forward with a given aim" `Quick (fun () ->
              Alcotest.(check PositionWithAim.testable)
                ""
                { horizontal = 5; depth = 25; aim = 5 }
                (PositionWithAim.move
                   { horizontal = 0; depth = 0; aim = 5 }
                   (Forward 5)));
        ] );
      ( "Summing Positions",
        [
          test_case "Empty list remains stationary" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 0; depth = 0 }
                (sum_positions Position.move { horizontal = 0; depth = 0 } []));
          test_case "Moves as given by the command" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 0; depth = 8 }
                (sum_positions Position.move
                   { horizontal = 0; depth = 0 }
                   [ Down 8 ]));
          test_case "Iterates over full length of list" `Quick (fun () ->
              Alcotest.(check Position.testable)
                ""
                { horizontal = 4; depth = 5 }
                (sum_positions Position.move
                   { horizontal = 0; depth = 0 }
                   [ Down 8; Up 4; Forward 4; Down 1 ]));
        ] );
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              Alcotest.(check int)
                "" 150
                (Position.distance_mul
                   (sum_positions Position.move
                      { horizontal = 0; depth = 0 }
                      [ Forward 5; Down 5; Forward 8; Up 3; Down 8; Forward 2 ])));
          test_case "Real (1)" `Quick (fun () ->
              Alcotest.(check int)
                "" 1840243
                (Position.distance_mul
                   (sum_positions Position.move
                      { horizontal = 0; depth = 0 }
                      (commands "inputs/day2.txt"))));
          test_case "Example (2)" `Quick (fun () ->
              Alcotest.(check int)
                "" 900
                (PositionWithAim.distance_mul
                   (sum_positions PositionWithAim.move
                      { horizontal = 0; depth = 0; aim = 0 }
                      [ Forward 5; Down 5; Forward 8; Up 3; Down 8; Forward 2 ])));
          test_case "Real (2)" `Quick (fun () ->
              Alcotest.(check int)
                "" 1727785422
                (PositionWithAim.distance_mul
                   (sum_positions PositionWithAim.move
                      { horizontal = 0; depth = 0; aim = 0 }
                      (commands "inputs/day2.txt"))));
        ] );
    ]
