module Coords = struct
  type t = {
      x: int;
      y: int;
    }

  let compare p1 p2 =
    match Int.compare p1.x p2.x with
    | 0 -> Int.compare p1.y p2.y
    | c -> c

  let ( + ) p1 p2 =
    { x = p1.x + p2.x; y = p1.y + p2.y }

  let neighbors p =
    List.map (( + ) p) [
        { x = -1; y = 0 }; { x = 0; y = -1 };
        { x = 1; y = 0 }; { x = 0; y = 1 }]

  let equal p1 p2 =
    p1.x = p2.x && p1.y = p2.y

  let hash p =
    Hashtbl.hash (p.x, p.y)
end

module CoordsSet = Set.Make (Coords)

let size matrix : Coords.t =
  { x = Array.length matrix.(0); y = Array.length matrix }

let get grid (p : Coords.t) =
  grid.(p.y).(p.x)

let in_bounds (sz : Coords.t) (p : Coords.t) =
  p.x >= 0 && p.y >= 0 && p.x < sz.x && p.y < sz.y

let _reachable_set grid visited p =
  let sz = size grid in
  let rec visit_neighbors set p =
    List.fold_left visit set (Coords.neighbors p)
  and visit set p =
    if not (in_bounds sz p) || get grid p = '#'
       || CoordsSet.mem p visited || CoordsSet.mem p set then
      set
    else
      visit_neighbors (CoordsSet.add p set) p in
  visit_neighbors CoordsSet.empty p

module State = struct
  type t = {
      visited : CoordsSet.t;
      p : Coords.t;
    }

  let equal s s' =
    CoordsSet.equal s.visited s'.visited && Coords.equal s.p s'.p

  let hash s =
    Hashtbl.hash (
      CoordsSet.fold (fun c accu -> Hashtbl.hash (accu, c)) s.visited 0,
      Coords.hash s.p)
end

module StateTable = Hashtbl.Make (State)

let find_path ~slippy grid =
  let sz = size grid in
  let target : Coords.t = { x = sz.x - 2; y = sz.y - 1 } in
(*
  let states = StateTable.create 16 in
*)
  let rec aux max_found (state : State.t) =
    if not (in_bounds sz state.p)
       || CoordsSet.mem state.p state.visited then
      max_found
    else
      begin
        let visited = CoordsSet.add state.p state.visited in
        if Coords.equal state.p target then
          if CoordsSet.cardinal visited - 1 > max_found then
            begin
              Format.eprintf "%d@." (CoordsSet.cardinal visited - 1);
              CoordsSet.cardinal visited - 1
            end
          else
            max_found
            (*max max_found (CoordsSet.cardinal visited - 1)*)
        else
          match get grid state.p with
          | '>' when slippy ->
             aux max_found { visited; p = Coords.(state.p + { x = 1; y = 0 }) }
          | 'v' when slippy ->
             aux max_found { visited; p = Coords.(state.p + { x = 0; y = 1 }) }
          | '.' | '>' | 'v' ->
             let neighbors = Coords.neighbors state.p in
             let neighbors = List.filter (fun p -> in_bounds sz p && get grid p != '#' && not (CoordsSet.mem p state.visited)) neighbors in
             begin match neighbors with
             | [p] -> aux max_found { visited; p }
             | neighbors ->
(*
                if StateTable.mem states state then
                  max_found
                else
                  begin
                    StateTable.add states state ();
*)
(*
                    let set = reachable_set grid visited state.p in
                    if not (CoordsSet.mem target set)
                       || CoordsSet.cardinal visited +
                            CoordsSet.cardinal set - 1 <= max_found then
                      max_found
                    else
*)
                      List.fold_left (fun max_found p -> aux max_found { visited; p })
                        max_found neighbors
(*
                  end
*)
             end
          | '#' -> max_found
          | _ -> assert false
      end in
  aux 0 { visited = CoordsSet.empty; p = { x = 1; y = 0 }}

let parse_line line =
  Array.of_seq (String.to_seq line)

let () =
  let grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 = find_path ~slippy:true grid in
  Printf.eprintf "Part 1: %d\n" result_part1;
  let result_part2 = find_path ~slippy:false grid in
  Printf.eprintf "Part 2: %d\n" result_part2
