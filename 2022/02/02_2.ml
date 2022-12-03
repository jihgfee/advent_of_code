
type shape = Rock | Paper | Scissor;;
type outcome = Lose | Draw | Win;;

let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let parse_shape c =
  match c with
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissor
  | _ -> assert false
in

let parse_outcome c =
  match c with
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> assert false
in

let compute_shape x y =
  match x, y with
  | Rock, Lose -> Scissor
  | Rock, Win -> Paper
  | Paper, Lose -> Rock
  | Paper, Win -> Scissor
  | Scissor, Lose -> Paper
  | Scissor, Win -> Rock
  | x, _ -> x
in

let rec parse_lines ls =
  match ls with
  | [] -> []
  | l :: ls -> let x = parse_shape l.[0] in
               let y = parse_outcome l.[2] in
               let z = compute_shape x y in
               (x,z)::parse_lines ls
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
