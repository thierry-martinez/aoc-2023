type coords = {
    x: int;
    y: int;
  }

let size matrix =
  { x = Array.length matrix.(0); y = Array.length matrix }

let offset { x; y } { x = dx; y = dy } =
  { x = x + dx; y = y + dy }

let in_bounds { x = width; y = height } { x; y } =
  x >= 0 && y >= 0 && x < width && y < height

let get matrix { x; y } =
  matrix.(y).(x)

let parse_line line =
  Array.of_seq (Seq.map
    (fun c -> Option.get (Common.int_of_digit c)) (String.to_seq line))

let turn_clockwise { x; y } =
  { x = y; y = x }

let turn_anticlockwise { x; y } =
  { x = -y; y = -x }

type visit = {
    from: coords;
    direction: coords;
    count_forward: int;
  }

let find_path ?min_turn ~max_forward grid from tgt =
  let visited = Hashtbl.create 16 in
  let grid_size = size grid in
  let visit_queue = ref [] in
  let initial direction =
    { from; direction; count_forward = 1 } in
  let push visit heat_loss =
    match Hashtbl.find_opt visited visit with
    | None ->
       visit_queue := visit :: !visit_queue;
       Hashtbl.add visited visit heat_loss
    | Some heat_loss' ->
       Hashtbl.replace visited visit (min heat_loss heat_loss') in
  let rec find_min min accu visit_queue =
    match visit_queue with
    | [] -> min
    | hd :: tl ->
       let min =
         match min with
         | Some (visit_min, _, _)
              when
                Hashtbl.find visited visit_min <=
                  Hashtbl.find visited hd ->
            min
         | _ ->
            Some (hd, accu, tl) in
       find_min min (hd :: accu) tl in
  let pop () =
    match find_min None [] !visit_queue with
    | None -> failwith "pop"
    | Some (visit, accu, tl) ->
       visit_queue := List.rev_append accu tl;
       visit in
  push (initial { x = 1; y = 0 }) 0;
  push (initial { x = 0; y = 1 }) 0;
  let rec loop () =
    let visit = pop () in
    let pos = offset visit.from visit.direction in
    if in_bounds grid_size pos then
      begin
        let heat_loss =
          Hashtbl.find visited visit + get grid pos in
        let turn_ok =
          match min_turn with
          | None -> true
          | Some min -> visit.count_forward >= min in
        if pos = tgt && turn_ok then
          heat_loss
        else
          begin
            if turn_ok then
              begin
                push {
                    from = pos;
                    direction = turn_anticlockwise visit.direction;
                    count_forward = 1
                  } heat_loss;
                push {
                    from = pos;
                    direction = turn_clockwise visit.direction;
                    count_forward = 1
                  } heat_loss;
              end;
            if visit.count_forward < max_forward then
              push {
                  from = pos;
                  direction = visit.direction;
                  count_forward = visit.count_forward + 1
                } heat_loss;
            loop ()
          end
      end
    else
      loop ()
  in
  loop ()

let () =
  let grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 =
    find_path grid { x = 0; y = 0 } (offset (size grid) { x = -1; y = -1 })
      ~max_forward:3 in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    find_path grid { x = 0; y = 0 } (offset (size grid) { x = -1; y = -1 })
      ~min_turn:4 ~max_forward:10 in
  Format.eprintf "Part 2: %d@." result_part2
