let first_column (board : 'a list list) : 'a list * 'a list list =
  let rec aux (board : 'a list list) (column : 'a list) (rest : 'a list list) :
      'a list * 'a list list =
    match board with
    | [] -> (List.rev column, List.rev rest)
    | [] :: tail -> aux tail column rest
    | [ value ] :: tail -> aux tail (value :: column) rest
    | (value :: row) :: tail -> aux tail (value :: column) (row :: rest)
  in
  aux board [] []

let transpose (board : 'a list list) : 'a list list =
  let rec aux (board : 'a list list) (columns : 'a list list) : 'a list list =
    if board == [] then List.rev columns
    else
      let column, next_board = first_column board in
      aux next_board (column :: columns)
  in
  aux board []

module BingoGrid = struct
  type line = { items : int list; found : int list; unfound : int list }

  type t = { rows : line list; cols : line list }

  let print grid =
    Printf.printf "g = ";
    List.iter
      (fun row ->
        Printf.printf "\t";
        List.iter
          (fun item ->
            if List.mem item row.unfound then Printf.printf "%4d" item
            else Printf.printf "    ")
          row.items;
        Printf.printf "\n")
      grid.rows;
    Printf.printf "\n"

  let of_board (board : int list list) : t =
    {
      rows = List.map (fun r -> { items = r; found = []; unfound = r }) board;
      cols =
        List.map
          (fun r -> { items = r; found = []; unfound = r })
          (transpose board);
    }

  let complete (grid : t) : bool =
    let row_found = List.exists (fun line -> line.unfound == []) grid.rows in
    let col_found = List.exists (fun line -> line.unfound == []) grid.cols in
    row_found || col_found

  let mark (n : int) (grid : t) : t =
    let mark_line ({ items; found; unfound } : line) : line =
      if List.mem n items then
        {
          items;
          found = n :: found;
          unfound = List.filter (fun i -> i != n) unfound;
        }
      else { items; found; unfound }
    in
    { rows = List.map mark_line grid.rows; cols = List.map mark_line grid.cols }

  let mark_until_complete (numbers : int list) (grid : t) : t * int =
    let rec aux numbers grid last_no =
      if complete grid then (grid, last_no)
      else
        match numbers with
        | [] -> assert false
        | head :: numbers -> aux numbers (mark head grid) head
    in
    aux numbers grid 0

  let score (grid : t) (last_called : int) : int =
    let total_unmarked (grid : t) : int =
      List.fold_left
        (fun acc line -> acc + List.fold_left ( + ) 0 line.unfound)
        0 grid.rows
    in
    last_called * total_unmarked grid

  let race (numbers : int list) (grids : t list) : int =
    let rec aux numbers grids last_no =
      match List.find_opt complete grids with
      | Some grid -> score grid last_no
      | None -> (
          match numbers with
          | [] -> assert false
          | head :: numbers -> aux numbers (List.map (mark head) grids) head)
    in
    aux numbers grids 0

  let race_backwards (numbers : int list) (grids : t list) : int =
    let rec aux numbers grids _ =
      match List.filter (fun g -> not (complete g)) grids with
      | [] -> assert false
      | [ grid ] -> race numbers [ grid ]
      | grids -> (
          match numbers with
          | [] -> assert false
          | head :: numbers -> aux numbers (List.map (mark head) grids) head)
    in
    aux numbers grids 0
end

let commands filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let parse lines =
  let rec group_grids lines current_grid finished_grids =
    match lines with
    | [] ->
        if current_grid == [] then finished_grids
        else BingoGrid.of_board (List.rev current_grid) :: finished_grids
    | "" :: lines ->
        if current_grid == [] then group_grids lines [] finished_grids
        else
          group_grids lines []
            (BingoGrid.of_board (List.rev current_grid) :: finished_grids)
    | line :: lines ->
        let line =
          String.split_on_char ' ' line
          |> List.filter (fun s -> String.length s > 0)
          |> List.map int_of_string
        in
        group_grids lines (line :: current_grid) finished_grids
  in
  match lines with
  | [] -> assert false
  | numbers :: cards ->
      ( String.split_on_char ',' numbers |> List.map int_of_string,
        group_grids cards [] [] )

let () =
  let open Alcotest in
  run "Day4"
    [
      ( "Getting the first column of a matrix",
        [
          test_case "Returns nothing for an empty list" `Quick (fun () ->
              Alcotest.(check (pair (list int) (list (list int))))
                "" ([], []) (first_column []));
          test_case "Returns all elements of a columnar vector" `Quick
            (fun () ->
              Alcotest.(check (pair (list int) (list (list int))))
                ""
                ([ 1; 2; 3 ], [])
                (first_column [ [ 1 ]; [ 2 ]; [ 3 ] ]));
          test_case "Returns first element of a row vector" `Quick (fun () ->
              Alcotest.(check (pair (list int) (list (list int))))
                ""
                ([ 1 ], [ [ 2; 3 ] ])
                (first_column [ [ 1; 2; 3 ] ]));
          test_case "Returns rest elements correctly" `Quick (fun () ->
              Alcotest.(check (pair (list int) (list (list int))))
                ""
                ([ 1; 2; 3 ], [ [ 8; 9 ]; [ 10; 11 ]; [ 12; 13 ] ])
                (first_column [ [ 1; 8; 9 ]; [ 2; 10; 11 ]; [ 3; 12; 13 ] ]));
        ] );
      ( "Transposing a matrix",
        [
          test_case "Returns nothing for an empty list" `Quick (fun () ->
              Alcotest.(check (list (list int))) "" [] (transpose []));
          test_case "Rotates a columnar vector" `Quick (fun () ->
              Alcotest.(check (list (list int)))
                "" [ [ 1; 2; 3 ] ]
                (transpose [ [ 1 ]; [ 2 ]; [ 3 ] ]));
          test_case "Rotates a row vector" `Quick (fun () ->
              Alcotest.(check (list (list int)))
                "" [ [ 1 ]; [ 2 ]; [ 3 ] ]
                (transpose [ [ 1; 2; 3 ] ]));
          test_case "Rotates a full matrix" `Quick (fun () ->
              Alcotest.(check (list (list int)))
                ""
                [ [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 3; 6; 9 ] ]
                (transpose [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]));
        ] );
      ( "Calculate board score",
        [
          test_case "Returns correct score for 5x5 board" `Quick (fun () ->
              let grid =
                BingoGrid.of_board
                  [
                    [ 14; 21; 17; 24; 4 ];
                    [ 10; 16; 15; 9; 19 ];
                    [ 18; 8; 23; 26; 20 ];
                    [ 22; 11; 13; 6; 5 ];
                    [ 2; 0; 12; 3; 7 ];
                  ]
              in
              let grid, score =
                BingoGrid.mark_until_complete
                  [ 7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24 ]
                  grid
              in
              Alcotest.(check int) "" 4512 (BingoGrid.score grid score));
        ] );
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              let grids =
                [
                  BingoGrid.of_board
                    [
                      [ 22; 13; 17; 11; 0 ];
                      [ 8; 2; 23; 4; 24 ];
                      [ 21; 9; 14; 16; 7 ];
                      [ 6; 10; 3; 18; 5 ];
                      [ 1; 12; 20; 15; 19 ];
                    ];
                  BingoGrid.of_board
                    [
                      [ 3; 15; 0; 2; 22 ];
                      [ 9; 18; 13; 17; 5 ];
                      [ 19; 8; 7; 25; 23 ];
                      [ 20; 11; 10; 24; 4 ];
                      [ 14; 21; 16; 12; 6 ];
                    ];
                  BingoGrid.of_board
                    [
                      [ 14; 21; 17; 24; 4 ];
                      [ 10; 16; 15; 9; 19 ];
                      [ 18; 8; 23; 26; 20 ];
                      [ 22; 11; 13; 6; 5 ];
                      [ 2; 0; 12; 3; 7 ];
                    ];
                ]
              in
              Alcotest.(check int)
                "" 4512
                (BingoGrid.race
                   [ 7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24 ]
                   grids));
          test_case "Real (1)" `Quick (fun () ->
              let numbers, grids = commands "inputs/day4.txt" |> parse in
              Alcotest.(check int) "" 74320 (BingoGrid.race numbers grids));
          test_case "Example (2)" `Quick (fun () ->
              let grids =
                [
                  BingoGrid.of_board
                    [
                      [ 22; 13; 17; 11; 0 ];
                      [ 8; 2; 23; 4; 24 ];
                      [ 21; 9; 14; 16; 7 ];
                      [ 6; 10; 3; 18; 5 ];
                      [ 1; 12; 20; 15; 19 ];
                    ];
                  BingoGrid.of_board
                    [
                      [ 3; 15; 0; 2; 22 ];
                      [ 9; 18; 13; 17; 5 ];
                      [ 19; 8; 7; 25; 23 ];
                      [ 20; 11; 10; 24; 4 ];
                      [ 14; 21; 16; 12; 6 ];
                    ];
                  BingoGrid.of_board
                    [
                      [ 14; 21; 17; 24; 4 ];
                      [ 10; 16; 15; 9; 19 ];
                      [ 18; 8; 23; 26; 20 ];
                      [ 22; 11; 13; 6; 5 ];
                      [ 2; 0; 12; 3; 7 ];
                    ];
                ]
              in
              Alcotest.(check int)
                "" 1924
                (BingoGrid.race_backwards
                   [ 7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24; 10; 16; 13 ]
                   grids));
          test_case "Real (2)" `Quick (fun () ->
              let numbers, grids = commands "inputs/day4.txt" |> parse in
              Alcotest.(check int)
                "" 17884
                (BingoGrid.race_backwards numbers grids));
        ] );
    ]
