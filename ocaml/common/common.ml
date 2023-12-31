let sum seq =
  Seq.fold_left ( + ) 0 seq

let input_lines () =
  Seq.of_dispenser (fun () ->
    try Some (input_line stdin)
    with End_of_file -> None)

let int_of_digit c =
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

let rec break p list =
  match list with
  | [] -> [], []
  | hd :: tl ->
     if p hd then
       [], list
     else
       let left, right = break p tl in
       hd :: left, right

let rec split p list =
  let hd, tl = break p list in
  match tl with
  | [] -> [hd]
  | _ :: tl -> hd :: split p tl
