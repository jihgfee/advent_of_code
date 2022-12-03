let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let explode s = List.init (String.length s) (String.get s) in

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

let rec parse_lines ls =
  match ls with
  | [] -> []
  | l::ls -> let n = (List.length l) / 2 in
             (take n l, drop n l) :: parse_lines ls
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

let compare_char c1 c2 = Int.compare (eval_char c1) (eval_char c2) in

let rec find_duplicate xs ys =
  match xs, ys with
  | x::xs, y::ys -> let z = compare_char x y in
                    if z = 0 then x
                    else if z > 0 then find_duplicate (x::xs) ys
                    else find_duplicate xs (y::ys)
  | _, _ -> assert false
in

let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum xs
in

let ls = read_lines () in
let xs = parse_lines (List.map explode ls) in
let sorted_xs = List.map (fun (ys,zs) -> (List.sort compare_char ys, List.sort compare_char zs)) xs in
let dups = List.map (fun (x,y) -> find_duplicate x y) sorted_xs in
let result = sum (List.map eval_char dups) in
Printf.printf "%i\n" result;
