module Coordinate = struct
  type t = int * int

  let of_points (x : int) (y : int) : t = (x, y)

  let compare ((x0, y0) : t) ((x1, y1) : t) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module CoordinateMap = Map.Make (Coordinate)

module Line = struct
  type t = Coordinate.t * Coordinate.t

  let of_points (x0 : int) (y0 : int) (x1 : int) (y1 : int) : t =
    (Coordinate.of_points x0 y0, Coordinate.of_points x1 y1)

  let of_coordinates coord1 coord2 : t = (coord1, coord2)

  let parse s = Scanf.sscanf s "%d,%d -> %d,%d" of_points

  let horizontal_or_vertical (((x0, y0), (x1, y1)) : t) = x0 == x1 || y0 == y1

  let as_points (((x0, y0), (x1, y1)) : t) : Coordinate.t List.t =
    let range first last =
      if last >= first then
        List.init (1 + last - first) Fun.id |> List.map (( + ) first)
      else List.init (1 + first - last) (fun x -> -x) |> List.map (( + ) first)
    in
    if x0 == x1 then
      range y0 y1 |> List.map (fun y -> Coordinate.of_points x0 y)
    else if y0 == y1 then
      range x0 x1 |> List.map (fun x -> Coordinate.of_points x y0)
    else List.map2 Coordinate.of_points (range x0 x1) (range y0 y1)
end

let insert_points points =
  let updater initial =
    match initial with Some n -> Some (n + 1) | None -> Some 1
  in
  let folder map point = CoordinateMap.update point updater map in
  List.fold_left folder CoordinateMap.empty points

let generate_points lines = List.concat_map Line.as_points lines

let count_crossings map =
  CoordinateMap.fold
    (fun _ crossings count -> if crossings >= 2 then count + 1 else count)
    map 0

let input filename =
  let channel = open_in filename in
  let stream =
    Stream.from (fun _ ->
        try Some (input_line channel) with End_of_file -> None)
  in
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  !result |> List.rev |> List.map Line.parse

let () =
  let open Alcotest in
  run "Day5"
    [
      ( "Problem Cases",
        [
          test_case "Example (1)" `Quick (fun () ->
              let lines =
                [
                  Line.of_coordinates (Coordinate.of_points 0 9)
                    (Coordinate.of_points 5 9);
                  Line.of_coordinates (Coordinate.of_points 8 0)
                    (Coordinate.of_points 0 8);
                  Line.of_coordinates (Coordinate.of_points 9 4)
                    (Coordinate.of_points 3 4);
                  Line.of_coordinates (Coordinate.of_points 2 2)
                    (Coordinate.of_points 2 1);
                  Line.of_coordinates (Coordinate.of_points 7 0)
                    (Coordinate.of_points 7 4);
                  Line.of_coordinates (Coordinate.of_points 6 4)
                    (Coordinate.of_points 2 0);
                  Line.of_coordinates (Coordinate.of_points 0 9)
                    (Coordinate.of_points 2 9);
                  Line.of_coordinates (Coordinate.of_points 3 4)
                    (Coordinate.of_points 1 4);
                  Line.of_coordinates (Coordinate.of_points 0 0)
                    (Coordinate.of_points 8 8);
                  Line.of_coordinates (Coordinate.of_points 5 5)
                    (Coordinate.of_points 8 2);
                ]
              in
              Alcotest.(check int)
                "" 5
                (lines
                |> List.filter Line.horizontal_or_vertical
                |> generate_points |> insert_points |> count_crossings));
          test_case "Real (1)" `Quick (fun () ->
              let lines = input "inputs/day5.txt" in
              Alcotest.(check int)
                "" 8622
                (lines
                |> List.filter Line.horizontal_or_vertical
                |> generate_points |> insert_points |> count_crossings));
          test_case "Example (2)" `Quick (fun () ->
              let lines =
                [
                  Line.of_coordinates (Coordinate.of_points 0 9)
                    (Coordinate.of_points 5 9);
                  Line.of_coordinates (Coordinate.of_points 8 0)
                    (Coordinate.of_points 0 8);
                  Line.of_coordinates (Coordinate.of_points 9 4)
                    (Coordinate.of_points 3 4);
                  Line.of_coordinates (Coordinate.of_points 2 2)
                    (Coordinate.of_points 2 1);
                  Line.of_coordinates (Coordinate.of_points 7 0)
                    (Coordinate.of_points 7 4);
                  Line.of_coordinates (Coordinate.of_points 6 4)
                    (Coordinate.of_points 2 0);
                  Line.of_coordinates (Coordinate.of_points 0 9)
                    (Coordinate.of_points 2 9);
                  Line.of_coordinates (Coordinate.of_points 3 4)
                    (Coordinate.of_points 1 4);
                  Line.of_coordinates (Coordinate.of_points 0 0)
                    (Coordinate.of_points 8 8);
                  Line.of_coordinates (Coordinate.of_points 5 5)
                    (Coordinate.of_points 8 2);
                ]
              in
              Alcotest.(check int)
                "" 12
                (lines |> generate_points |> insert_points |> count_crossings));
          test_case "Real (2)" `Quick (fun () ->
              let lines = input "inputs/day5.txt" in
              Alcotest.(check int)
                "" 22037
                (lines |> generate_points |> insert_points |> count_crossings));
        ] );
    ]
