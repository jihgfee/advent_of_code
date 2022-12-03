
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

let parse_shape_alt c =
  match c with
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissor
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

let rec parse_lines t ls =
  match ls with
  | [] -> []
  | l :: ls -> let x = parse_shape l.[0] in
               (x,t x l.[2])::parse_lines t ls
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

let tactic1 = fun _ y -> parse_shape_alt y in
let tactic2 = fun x y -> compute_shape x (parse_outcome y) in

let ls = read_lines () in
let sum1 = List.fold_left
            (fun sum xy -> sum + eval_round xy) 0 (parse_lines tactic1 ls) in
Printf.printf "%i\n" sum1;

let sum2 = List.fold_left
            (fun sum xy -> sum + eval_round xy) 0 (parse_lines tactic2 ls) in
Printf.printf "%i\n" sum2
