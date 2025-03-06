let explode s = List.init (String.length s) (String.get s) in

let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then [] else x :: take (n-1) xs
in

let rec contains_duplicates cs =
  match cs with
  | c1::c2::cs -> if c1 = c2 then true else contains_duplicates (c2::cs)
  | _          -> false
in

let rec find_marker n i cs =
  if contains_duplicates (List.sort Char.compare (take n cs))
  then find_marker n (i+1) (List.tl cs)
  else i+n
in

let s = read_line () in
let i1 = find_marker 4 0 (explode s) in
let i2 = find_marker 14 0 (explode s) in
Printf.printf "Part 1: %i\n" i1;
Printf.printf "Part 2: %i\n" i2;
