
type shape = Rock | Paper | Scissor;;

let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let parse_char c =
  match c with
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissor
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissor
  | _ -> assert false
in

let rec parse_lines ls =
  match ls with
  | [] -> []
  | l :: ls -> (parse_char l.[0],parse_char l.[2])::parse_lines ls
in

let shape_score x =
  match x with
  | Rock -> 1
  | Paper -> 2
  | Scissor -> 3
in

let eval_round (x,y) =
  shape_score y +
  match x,y with
  | Rock, Paper -> 6
  | Paper, Scissor -> 6
  | Scissor, Rock -> 6
  | Paper, Rock -> 0
  | Scissor, Paper -> 0
  | Rock, Scissor -> 0
  | _, _ -> 3
in

let ls = read_lines () in
let xs = parse_lines ls in
let sum = List.fold_left (fun sum xy -> sum + eval_round xy) 0 xs in

Printf.printf "%i" sum
