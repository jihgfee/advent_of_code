let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let explode s = List.init (String.length s) (String.get s) in
let implode cs = String.of_seq (List.to_seq cs) in

let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then [] else x :: take (n-1) xs
in

let rec drop n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then (x::xs) else drop (n-1) xs
in

let is_lowercase c = (c = Char.lowercase_ascii c) in
let is_uppercase c = (c = Char.uppercase_ascii c) in

let lowercase_offset = 96 in
let uppercase_offset = 64 in

let eval_char c =
  if is_lowercase c then Char.code c - lowercase_offset
  else if is_uppercase c then Char.code c - uppercase_offset + 26
  else assert false
in

let eval_int x =
  if x <= 26 then Char.chr (x+lowercase_offset)
  else Char.chr ((x - 26) + uppercase_offset)
in

(* TODO: parse strategy *)
let rec parse_lines_pre ls =
  match ls with
  | [] -> []
  | l::ls -> let cs = explode l in
             let n = (List.length cs) / 2 in
             (implode (take n cs)) :: (implode (drop n cs)) :: parse_lines_pre ls
in

let rec parse_lines n ls =
  match ls with
  | [] -> []
  | ls -> List.map (fun l -> List.map eval_char (explode l)) (take n ls) :: parse_lines n (drop n ls)
in

let rec remove_duplicates xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> if x = y then remove_duplicates (y::xs) else x :: remove_duplicates (y::xs)
in

let check_same xs = List.filter (fun x -> x != List.hd xs) xs = [] in

let rec find_duplicate n xs =
  if check_same (take n xs) then List.hd xs else find_duplicate n (List.tl xs)
in

let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum xs
in

let ls = read_lines () in

(* Part 1 *)
let xs = parse_lines 2 (parse_lines_pre ls) in
let xs' = List.map (fun x -> List.map (fun y -> remove_duplicates (List.sort Int.compare y)) x) xs in
let xs'' = List.map (fun x -> List.sort Int.compare (List.flatten x)) xs' in
let dups = List.map (find_duplicate 2) xs'' in
let result = sum dups in
Printf.printf "Part 1: %i \n" result;

(* Part 2 *)
let xs = parse_lines 3 ls in
let xs' = List.map (fun x -> List.map (fun y -> remove_duplicates (List.sort Int.compare y)) x) xs in
let xs'' = List.map (fun x -> List.sort Int.compare (List.flatten x)) xs' in
let dups = List.map (find_duplicate 3) xs'' in
let result = sum dups in
Printf.printf "Part 2: %i\n" result;
